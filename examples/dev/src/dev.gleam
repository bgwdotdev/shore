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
    shore.Spec(
      init:,
      view:,
      update:,
      exit:,
      keybinds: shore.default_keybinds(),
      redraw: shore.OnUpdate,
    )
    |> shore.start
  //tick(ui)
  process.receive_forever(exit)
}

fn tick(ui: process.Subject(shore.Event(Msg))) {
  process.send(ui, shore.cmd(Tick))
  process.sleep(1000)
  tick(ui)
}

// MODEL 

type Model {
  Model(counter: Int, hi: String, bye: String, csv: List(List(String)))
}

fn init() -> #(Model, List(fn() -> Msg)) {
  let csv = [
    ["name", "age", "date"],
    ["bob", "20", "2025-01-01"],
    ["alice the quick brown fox", "30", "2025-03-04"],
    ["jane", "54", "2024-10-10"],
    ["phil", "33", "2025-02-11"],
  ]
  let model =
    Model(counter: 0, hi: "abcdefghijklmnopqrstuvwxyz", bye: "abcdarsd", csv:)
  #(model, [])
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
  shore.DivCol([
    "HELLO WORLD" |> shore.Text(None, None),
    shore.HR,
    shore.Input("hi:", model.hi, shore.Px(20), SetHi, shore.Simple),
    shore.BR,
    model.hi |> shore.Text(None, None),
    shore.BR,
    shore.Input("bye:", model.bye, shore.Px(25), SetBye, shore.Simple),
    shore.BR,
    model.bye |> shore.Text(None, None),
    shore.BR,
    shore.TableKV(50, model.csv),
    shore.BR,
    shore.Text("hi", None, None),
    //shore.Progress(shore.Px(20), 100, model.counter, shore.Blue),
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
    shore.DivRow([
      shore.Button("Increment", key.Char("a"), Increment),
      shore.Button("Decrement", key.Char("b"), Decrement),
    ]),
    case model.counter {
      x if x > 10 && x < 20 -> shore.Button("reset", key.Char("r"), SendReset)
      x if x > 20 -> shore.Button("dd", key.Char("d"), Set(0))
      x -> shore.Text("x", Some(shore.Red), None)
    },
    shore.BR,
    shore.Graph(shore.Px(60), 7, [
      1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0,
      6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0,
      1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0,
      6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0,
      1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0,
      6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0,
      1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0,
      6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0,
      1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0,
      6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0,
      1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0,
    ]),
    shore.Text("hi", None, None),
  ])
}

// CMDS

fn reset() -> Msg {
  process.sleep(10_000)
  Reset
}
