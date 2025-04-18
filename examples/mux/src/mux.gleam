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
    shore.Spec(init:, update:, view:, exit:, keybinds: shore.default_keybinds())
    |> shore.start
  process.receive_forever(exit)
}

// MODEL

pub type Model {
  Model(
    count: Int,
    last_modified: Int,
    cmd: String,
    output: List(Output),
    term: Term,
  )
}

fn init() -> #(Model, List(fn() -> Msg)) {
  let model =
    Model(
      count: 0,
      last_modified: 0,
      cmd: "",
      output: [help()],
      term: Term(0, "", [help()], None, shore.Horizontal),
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
    term: Option(Term),
    split: shore.Direction,
  )
}

// ERROR

pub opaque type Err {
  Cmd(Output)
  Pop(Nil)
  NotInt(Nil)
}

// UPDATE

pub type Msg {
  NoOp
  SetCommand(Int, String)
  SendCommand(Int)
  ReceiveCommand(Int, Result(Output, Err))
  Clear(Int)
  CreateSplit(Int, shore.Direction)
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
    CreateSplit(_id, direction) -> {
      let count = model.count + 1
      let term =
        update_term(model.count, model.term, fn(term) {
          Term(..term, term: Some(Term(count, "", [], None, direction)))
        })
      #(Model(..model, count:, term:), [])
    }
  }
}

fn update_term(id: Int, term: Term, func: fn(Term) -> Term) -> Term {
  case id == term.id {
    True -> func(term)
    False ->
      case term.term {
        Some(child) -> Term(..term, term: Some(update_term(id, child, func)))
        None -> term
      }
  }
}

fn update_term_cmd(
  id: Int,
  term: Term,
  func: fn(Term) -> Term,
) -> #(Term, String) {
  case id == term.id {
    True -> #(func(term), term.cmd)
    False ->
      case term.term {
        Some(child) -> {
          let #(up, cmd) = update_term_cmd(id, child, func)
          #(Term(..term, term: Some(up)), cmd)
        }
        None -> #(term, "echo 'how did I get here?'")
      }
  }
}

// VIEW

fn view(model: Model) -> shore.Node(Msg) {
  shore.Split(shore.Split2(
    shore.Horizontal,
    shore.Ratio2(1, 100),
    shore.Split1(view_keybinds(model)),
    shore.Split1(shore.Split(view_term(model.term))),
  ))
}

fn view_keybinds(model: Model) -> shore.Node(Msg) {
  shore.Div(
    [
      shore.KeyBind(key.Enter, SendCommand(model.last_modified)),
      shore.KeyBind(key.Char("L"), Clear(model.last_modified)),
      shore.KeyBind(
        key.Char("H"),
        CreateSplit(model.last_modified, shore.Horizontal),
      ),
      shore.KeyBind(
        key.Char("V"),
        CreateSplit(model.last_modified, shore.Vertical),
      ),
    ],
    shore.Row,
  )
}

fn view_term(term: Term) -> shore.Splits(Msg) {
  let #(size, other, split) = case term.term {
    Some(term) -> #(shore.Ratio2(50, 50), view_term(term), term.split)
    None -> #(shore.Ratio2(100, 10), view_none(), shore.Horizontal)
  }
  shore.Split2(split, size, shore.Split1(view_term_output(term)), other)
}

fn view_term_output(term: Term) -> shore.Node(Msg) {
  let prompt =
    shore.Input(
      int.to_string(term.id) <> "$",
      term.cmd,
      shore.Fixed(40),
      fn(str) { SetCommand(term.id, str) },
      shore.Simple,
    )
  [prompt, ..list.map(term.output, format_output)]
  |> list.reverse
  |> shore.Box(term.id |> int.to_string |> Some)
}

fn view_none() -> shore.Splits(Msg) {
  shore.Split1(shore.Div([], shore.Col))
}

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
  J -> cycle input focus forward
  K -> cycle input focus back
  I -> insert mode
  Esc -> normal mode
  Enter -> submit command (normal mode)
  "
  Output(status: 0, text:, cmd: "help")
}
