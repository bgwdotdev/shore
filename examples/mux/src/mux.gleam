import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import shore
import shore/key

// MAIN

pub fn main() {
  let exit = process.new_subject()
  let _shore =
    shore.Spec(
      init:,
      update:,
      view:,
      exit:,
      keybinds: shore.vim_keybinds(),
      redraw: shore.OnTimer(17),
    )
    |> shore.start
  process.receive_forever(exit)
}

// MODEL

pub type Model {
  Model(
    rows: Int,
    cols: Int,
    count: Int,
    last_modified: Int,
    cmd: String,
    output: List(Output),
    term: List(Term),
  )
}

type Direction {
  Horizontal
  Vertical
}

fn init() -> #(Model, List(fn() -> Msg)) {
  let model =
    Model(
      rows: 0,
      cols: 0,
      count: 0,
      last_modified: 0,
      cmd: "",
      output: [help()],
      term: [Term(0, "", [help()], #(0, 0), #(0, 0))],
    )
  let cmd = []
  #(model, cmd)
}

pub opaque type Output {
  Output(status: Int, text: String, cmd: String)
}

pub opaque type Term {
  Term(
    id: Int,
    cmd: String,
    output: List(Output),
    row: #(Int, Int),
    col: #(Int, Int),
  )
}

// ERROR

pub opaque type Err {
  Cmd(Output)
  Pop(Nil)
  NotInt(Nil)
}

// UPDATE

pub opaque type Msg {
  NoOp
  SetCommand(Int, String)
  SendCommand(Int)
  ReceiveCommand(Int, Result(Output, Err))
  Clear(Int)
  CreateSplit(Int, Direction)
}

fn update(model: Model, msg: Msg) -> #(Model, List(fn() -> Msg)) {
  case msg {
    NoOp -> #(model, [])
    SetCommand(id, cmd) -> {
      let term = update_term(id, model.term, fn(term) { Term(..term, cmd:) })
      #(Model(..model, last_modified: id, term:), [])
    }
    SendCommand(id) -> {
      let #(term, cmd) =
        update_term_cmd(id, model.term, fn(term) { Term(..term, cmd: "") })
      let cmd = case cmd {
        "" -> "echo ''"
        x -> x
      }
      #(Model(..model, term:), [send_command(id, cmd)])
    }
    ReceiveCommand(id, Ok(output)) | ReceiveCommand(id, Error(Cmd(output))) -> {
      let term =
        update_term(id, model.term, fn(term) {
          Term(..term, output: [output, ..term.output])
        })
      #(Model(..model, term:), [])
    }
    ReceiveCommand(_, Error(err)) -> {
      panic as { "put in error handling " <> string.inspect(err) }
    }
    Clear(id) -> {
      let term =
        update_term(id, model.term, fn(term) { Term(..term, output: []) })
      #(Model(..model, term:), [])
    }
    CreateSplit(id, direction) -> {
      let new_model = {
        use term <- result.map(
          list.filter(model.term, fn(t) { t.id == id }) |> list.first,
        )
        case direction {
          Horizontal ->
            case term.col.1 == model.cols {
              True -> {
                let cols = model.cols + 1
                let count = model.count + 1
                let new_term =
                  Term(count, "", [], #(term.row.0, term.row.0), #(cols, cols))
                Model(..model, count:, cols:, term: [new_term, ..model.term])
              }
              False -> {
                let cols = model.cols + 1
                let count = model.count + 1
                let terms =
                  list.map(model.term, fn(t) {
                    case t.row.0 == term.row.0 {
                      True -> t
                      False ->
                        case t.col.1 == model.cols {
                          True -> Term(..t, col: #(t.col.0, cols))
                          False -> t
                        }
                    }
                  })
                let new_term =
                  Term(count, "", [], #(term.row.0, term.row.0), #(cols, cols))

                Model(..model, count:, cols:, term: [new_term, ..terms])
              }
            }
          Vertical -> {
            let rows = model.rows + 1
            let count = model.count + 1
            let new_term = Term(count, "", [], #(rows, rows), #(0, model.cols))
            Model(..model, count:, rows:, term: [new_term, ..model.term])
          }
        }
      }
      case new_model {
        Ok(model) -> #(model, [])
        Error(_) -> #(model, [])
      }
    }
    //CreateSplit(_id, direction) -> {
    //  let count = model.count + 1
    //  // TODO: implement split logic
    //  let pos = #(0, 0)
    //  let term =
    //    update_term(model.count, model.term, fn(term) {
    //      Term(..term, term: Some(Term(count, "", [], None, pos)))
    //    })
    //  #(Model(..model, count:, term:), [])
    //}
  }
}

fn update_term(id: Int, terms: List(Term), func: fn(Term) -> Term) -> List(Term) {
  fn(term: Term) {
    case term.id == id {
      True -> func(term)
      False -> term
    }
  }
  |> list.map(terms, _)
  //case id == term.id {
  //  True -> func(term)
  //  False ->
  //    case term.term {
  //      Some(child) -> Term(..term, term: Some(update_term(id, child, func)))
  //      None -> term
  //    }
  //}
}

fn update_term_cmd(
  id: Int,
  terms: List(Term),
  func: fn(Term) -> Term,
) -> #(List(Term), String) {
  fn(acc: #(List(Term), String), term: Term) {
    case term.id == id {
      True -> #([func(term), ..acc.0], term.cmd)
      False -> #([term, ..acc.0], acc.1)
    }
  }
  |> list.fold(terms, #([], ""), _)
}

// VIEW

fn view(model: Model) -> shore.Node(Msg) {
  shore.Layouts(
    shore.Grid(
      gap: 1,
      rows: list.repeat(shore.Fill, model.rows + 1),
      columns: list.repeat(shore.Fill, model.cols + 1),
      cells: [view_keybinds(model), ..list.map(model.term, view_term_output)],
    ),
  )
}

fn view_keybinds(model: Model) -> shore.Cell(Msg) {
  shore.Cell(
    shore.DivRow([
      shore.KeyBind(key.Enter, SendCommand(model.last_modified)),
      shore.KeyBind(key.Char("L"), Clear(model.last_modified)),
      shore.KeyBind(key.Char("H"), CreateSplit(model.last_modified, Horizontal)),
      shore.KeyBind(key.Char("V"), CreateSplit(model.last_modified, Vertical)),
    ]),
    #(0, 0),
    #(0, 0),
  )
}

//fn view_term(term: Term) -> shore.Splits(Msg) {
//  let #(size, other, split) = case term.term {
//    Some(term) -> #(
//      shore.Ratio2(shore.Pct(50), shore.Pct(50)),
//      view_term(term),
//      term.split,
//    )
//    None -> #(
//      shore.Ratio2(shore.Pct(100), shore.Pct(10)),
//      view_none(),
//      shore.Horizontal,
//    )
//  }
//  shore.Split2(split, size, shore.Split1(view_term_output(term)), other)
//}

fn view_term_output(term: Term) -> shore.Cell(Msg) {
  let prompt =
    shore.Input(
      int.to_string(term.id) <> "$",
      term.cmd,
      shore.Fill,
      fn(str) { SetCommand(term.id, str) },
      shore.Simple,
    )
  [prompt, ..list.map(term.output, format_output)]
  |> list.reverse
  |> shore.Box(term.id |> int.to_string |> Some)
  |> shore.Cell(term.row, term.col)
}

//fn view_none() -> shore.Splits(Msg) {
//  shore.Split1(shore.Div([], shore.Col))
//}

fn format_output(output: Output) -> shore.Node(Msg) {
  let out =
    "> " <> string.trim(output.cmd) <> "\n" <> string.trim(output.text) <> "\n"
  case output.status {
    0 -> shore.TextMulti(out, None, None)
    _ -> shore.TextMulti(out, Some(shore.Red), None)
  }
}

// CMD

@external(erlang, "os", "cmd")
fn command(cmd: Charlist) -> Charlist

fn send_command(id: Int, cmd: String) -> fn() -> Msg {
  fn() {
    { cmd <> ";printf $?" }
    |> charlist.from_string
    |> command
    |> charlist.to_string
    |> string.reverse
    |> string.pop_grapheme
    |> result.map_error(Pop)
    |> result.then(fn(str) {
      case str.0 {
        "0" -> Output(0, string.reverse(str.1), cmd) |> Ok
        "1" -> Output(1, string.reverse(str.1), cmd) |> Cmd |> Error
        x ->
          x
          |> int.parse
          |> result.map_error(NotInt)
          |> result.map(fn(i) { Output(i, string.reverse(str.1), cmd) })
      }
    })
    |> ReceiveCommand(id, _)
  }
}

// HELPERS

fn help() -> Output {
  let text =
    "
Welcome to mux, a simple terminal multiplexor inspired by tmux!

Keybinds:
  Q -> quit
  H -> create horizontal split
  V -> create vertical split
  D -> delete pane
  j -> cycle input focus forward
  k -> cycle input focus back
  i -> insert mode (normal mode)
  Esc -> normal mode (insert mode)
  Enter -> submit command (normal mode)
  "
  Output(status: 0, text:, cmd: "help")
}
