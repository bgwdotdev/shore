import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/pair
import shore

// MAIN

pub fn main() {
  shore.Spec(init:, view:, update:) |> shore.start
}

// MODEL 

type Model {
  Model(counter: Int)
}

fn init() -> #(Model, List(fn() -> Msg)) {
  let model = Model(counter: 0)
  #(model, [fn() { Set(10) }])
}

// UPDATE

type Msg {
  Increment
  Decrement
  SendReset
  Reset
  Set(Int)
}

fn update(model: Model, msg: Msg) -> #(Model, List(fn() -> Msg)) {
  case msg {
    Increment -> Model(model.counter + 1) |> pair.new([])
    Decrement -> Model(model.counter - 1) |> pair.new([])
    SendReset -> #(model, [reset])
    Reset -> #(Model(0), [])
    Set(i) -> #(Model(i), [])
  }
}

// VIEW

fn view(model: Model) -> shore.Node(Msg) {
  shore.Div(
    [
      "HELLO WORLD" |> shore.Text(None),
      shore.HR,
      shore.Input("hi"),
      shore.BR,
      shore.BR,
      shore.Input("bye"),
      shore.BR,
      shore.BR,
      shore.Input("try"),
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
