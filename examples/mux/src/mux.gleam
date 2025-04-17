import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
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
  Model(cmd: String, output: List(Output))
}

fn init() -> #(Model, List(fn() -> Msg)) {
  let model = Model(cmd: "", output: [])
  let cmd = []
  #(model, cmd)
}

pub opaque type Output {
  Output(status: Int, text: String, cmd: String)
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
  SetCommand(String)
  SendCommand
  ReceiveCommand(Result(Output, Err))
  Clear
}

fn update(model: Model, msg: Msg) -> #(Model, List(fn() -> Msg)) {
  case msg {
    NoOp -> #(model, [])
    SetCommand(cmd) -> #(Model(..model, cmd:), [])
    SendCommand -> {
      let cmd = case model.cmd {
        "" -> "echo ''"
        x -> x
      }
      #(Model(..model, cmd: ""), [send_command(cmd)])
    }
    ReceiveCommand(Ok(output)) | ReceiveCommand(Error(Cmd(output))) -> #(
      Model(..model, output: [output, ..model.output]),
      [],
    )
    ReceiveCommand(Error(err)) -> {
      panic as { "put in error handling " <> string.inspect(err) }
    }
    Clear -> #(Model(..model, output: []), [])
  }
}

// VIEW

fn view(model: Model) -> shore.Node(Msg) {
  shore.Split(shore.Split2(
    shore.Horizontal,
    shore.Ratio2(0, 100),
    shore.Split1(view_cmd(model)),
    shore.Split1(view_output(model)),
  ))
}

fn view_cmd(model: Model) -> shore.Node(Msg) {
  shore.Div(
    [shore.KeyBind(key.Enter, SendCommand), shore.KeyBind(key.Char("L"), Clear)],
    shore.Col,
  )
}

fn view_output(model: Model) -> shore.Node(Msg) {
  let prompt =
    shore.Input("$", model.cmd, shore.Fixed(40), SetCommand, shore.Simple)
  [prompt, ..list.map(model.output, format_output)]
  |> list.reverse
  |> shore.Box(0 |> int.to_string |> Some)
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

fn send_command(cmd: String) -> fn() -> Msg {
  fn() {
    { cmd <> ";printf $?" }
    |> charlist.from_string
    |> command
    |> charlist.to_string
    |> echo
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
    |> ReceiveCommand
  }
}
// HELPER
