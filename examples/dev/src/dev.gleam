import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/pair
import shore
import shore/key

// MAIN

pub fn main() {
  let exit = process.new_subject()
  let ui =
    shore.Spec(init:, view:, update:, exit:, keybinds: shore.default_keybinds())
    |> shore.start
  tick(ui)
  process.receive_forever(exit)
}

fn tick(ui: process.Subject(shore.Event(Msg))) {
  process.send(ui, shore.cmd(Tick))
  process.sleep(1000)
  tick(ui)
}

// MODEL 

type Model {
  Model(counter: Int, hi: String, bye: String)
}

fn init() -> #(Model, List(fn() -> Msg)) {
  let model =
    Model(counter: 0, hi: "abcdefghijklmnopqrstuvwxyz", bye: "abcdarsd")
  #(model, [fn() { Set(10) }])
}

// UPDATE

type Msg {
  NoOp
  FixMe(String)
  Increment
  Decrement
  SendReset
  Reset
  Set(Int)
  Tick
  SetHi(String)
  SetBye(String)
}

fn update(model: Model, msg: Msg) -> #(Model, List(fn() -> Msg)) {
  case msg {
    NoOp -> #(model, [])
    FixMe(_) -> #(model, [])
    Increment -> Model(..model, counter: model.counter + 1) |> pair.new([])
    Decrement -> Model(..model, counter: model.counter - 1) |> pair.new([])
    SendReset -> #(model, [reset])
    Reset -> #(Model(..model, counter: 0), [])
    Set(i) -> #(Model(..model, counter: i), [])
    Tick -> #(Model(..model, counter: model.counter + 1), [])
    SetHi(text) -> #(Model(..model, hi: text), [])
    SetBye(text) -> #(Model(..model, bye: text), [])
  }
}

// VIEW

fn view(model: Model) -> shore.Node(Msg) {
  shore.Div(
    [
      "HELLO WORLD" |> shore.Text(None, None),
      shore.HR,
      shore.Input(20, "hi", model.hi, SetHi),
      shore.BR,
      shore.BR,
      model.hi |> shore.Text(None, None),
      shore.BR,
      shore.BR,
      shore.Input(25, "bye", model.bye, SetBye),
      shore.BR,
      model.bye |> shore.Text(None, None),
      shore.BR,
      shore.Input(20, "try", "", FixMe),
      shore.BR,
      shore.BR,
      model.counter |> int.to_string |> shore.Text(Some(shore.Black), None),
      model.counter |> int.to_string |> shore.Text(Some(shore.Red), None),
      model.counter |> int.to_string |> shore.Text(Some(shore.Green), None),
      model.counter |> int.to_string |> shore.Text(Some(shore.Yellow), None),
      model.counter |> int.to_string |> shore.Text(Some(shore.Blue), None),
      model.counter |> int.to_string |> shore.Text(Some(shore.Magenta), None),
      model.counter |> int.to_string |> shore.Text(Some(shore.Cyan), None),
      model.counter |> int.to_string |> shore.Text(Some(shore.White), None),
      shore.BR,
      shore.Div(
        [
          shore.Button("++", key.Char("a"), Increment),
          shore.Button("-", key.Char("b"), Decrement),
        ],
        shore.Row,
      ),
      case model.counter {
        x if x > 10 && x < 20 -> shore.Button("reset", key.Char("r"), SendReset)
        x if x > 20 -> shore.Button("dd", key.Char("d"), Set(0))
        x -> shore.Text("x", Some(shore.Red), None)
      },
    ],
    shore.Col,
  )
}

// CMDS

fn reset() -> Msg {
  process.sleep(10_000)
  Reset
}
