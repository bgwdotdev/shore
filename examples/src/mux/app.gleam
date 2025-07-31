import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import shore
import shore/key
import shore/layout
import shore/style
import shore/ui

// MAIN

pub fn main() {
  let exit = process.new_subject()
  let _shore =
    shore.spec(
      init:,
      update:,
      view:,
      exit:,
      keybinds: shore.default_keybinds(),
      redraw: shore.on_timer(17),
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
  layout.grid(
    gap: 1,
    rows: list.repeat(style.Fill, model.rows + 1),
    cols: list.repeat(style.Fill, model.cols + 1),
    cells: [view_keybinds(model), ..list.map(model.term, view_term_output)],
  )
}

fn view_keybinds(model: Model) -> layout.Cell(Msg) {
  layout.cell(
    ui.row([
      ui.keybind(key.Char("L"), Clear(model.last_modified)),
      ui.keybind(key.Char("H"), CreateSplit(model.last_modified, Horizontal)),
      ui.keybind(key.Char("V"), CreateSplit(model.last_modified, Vertical)),
    ]),
    #(0, 0),
    #(0, 0),
  )
}

fn view_term_output(term: Term) -> layout.Cell(Msg) {
  let prompt =
    ui.input_submit(
      int.to_string(term.id) <> "$",
      term.cmd,
      style.Fill,
      fn(str) { SetCommand(term.id, str) },
      SendCommand(term.id),
      False,
    )
  [prompt, ..list.map(term.output, format_output)]
  |> list.reverse
  |> ui.box(term.id |> int.to_string |> Some)
  |> layout.cell(term.row, term.col)
}

fn format_output(output: Output) -> shore.Node(Msg) {
  let out =
    "> " <> string.trim(output.cmd) <> "\n" <> string.trim(output.text) <> "\n"
  case output.status {
    0 -> ui.text(out)
    _ -> ui.text_styled(out, Some(style.Red), None)
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
    |> result.try(fn(str) {
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

This is pretty jank.

Keybinds:
  Ctrl+x -> quit
  H -> create horizontal split
  V -> create vertical split
  Tab -> cycle input focus forward
  Shift+Tab -> cycle input focus back
  Escape -> clear focus
  Enter -> submit command
  "
  Output(status: 0, text:, cmd: "help")
}
