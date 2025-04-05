import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/pair
import shore

// MAIN

pub fn main() {
  let ui = shore.Spec(init:, view:, update:) |> shore.start
  tick(ui)
  process.sleep_forever()
}

fn tick(ui: process.Subject(shore.Event(Msg))) {
  process.send(ui, shore.cmd(Tick))
  process.sleep(1000)
  tick(ui)
}

// MODEL 

type Model {
  Model(counter: Int, hi: String)
}

fn init() -> #(Model, List(fn() -> Msg)) {
  let model = Model(counter: 0, hi: "")
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
    SetHi(text) -> #(Model(..model, hi: model.hi <> text), [])
  }
}

// VIEW

fn view(model: Model) -> shore.Node(Msg) {
  shore.Div(
    [
      "HELLO WORLD" |> shore.Text(None),
      shore.HR,
      shore.Input("hi", SetHi),
      shore.BR,
      shore.BR,
      model.hi |> shore.Text(None),
      shore.BR,
      shore.BR,
      shore.Input("bye", FixMe),
      shore.BR,
      shore.BR,
      shore.Input("try", FixMe),
      shore.BR,
      shore.BR,
      model.counter |> int.to_string |> shore.Text(Some(shore.Black)),
      model.counter |> int.to_string |> shore.Text(Some(shore.Red)),
      model.counter |> int.to_string |> shore.Text(Some(shore.Green)),
      model.counter |> int.to_string |> shore.Text(Some(shore.Yellow)),
      model.counter |> int.to_string |> shore.Text(Some(shore.Blue)),
      model.counter |> int.to_string |> shore.Text(Some(shore.Magenta)),
      model.counter |> int.to_string |> shore.Text(Some(shore.Cyan)),
      model.counter |> int.to_string |> shore.Text(Some(shore.White)),
      shore.BR,
      shore.Div(
        [shore.Button("++", "a", Increment), shore.Button("--", "b", Decrement)],
        shore.Row,
      ),
      case model.counter {
        x if x > 10 && x < 20 -> shore.Button("reset", "r", SendReset)
        x if x > 20 -> shore.Button("dd", "d", Set(0))
        x -> shore.Text("x", Some(shore.Red))
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
