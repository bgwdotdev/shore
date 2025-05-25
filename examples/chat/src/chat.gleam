////shore.Px(50),

import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import shore
import shore/key
import shore/ssh

// MAIN
//pub fn main() -> Nil {
//  let assert Ok(_) = ssh.serve()
//  process.sleep_forever()
//}

pub fn main() {
  let exit = process.new_subject()
  let spec =
    shore.Spec(
      init:,
      update:,
      view:,
      exit:,
      keybinds: shore.default_keybinds(),
      redraw: shore.OnTimer(16),
    )
  let assert Ok(_actor) = spec |> shore.start
  let assert Ok(_) = ssh.serve(spec)
  exit |> process.receive_forever
}

// MODEL

pub opaque type Model {
  Model(counter: Int)
}

fn init() -> #(Model, List(fn() -> Msg)) {
  let model = Model(counter: 0)
  let cmds = []
  #(model, cmds)
}

// UPDATE

pub opaque type Msg {
  Increment
  Decrement
}

fn update(model: Model, msg: Msg) -> #(Model, List(fn() -> Msg)) {
  case msg {
    Increment -> #(Model(counter: model.counter + 1), [])
    Decrement -> #(Model(counter: model.counter - 1), [])
  }
}

// VIEW

fn view(model: Model) -> shore.Node(Msg) {
  shore.Layouts(shore.layout_center(
    shore.DivCol([
      shore.TextMulti(
        "keybinds

i: increments
d: decrements
ctrl+x: exits
      ",
        None,
        None,
      ),
      shore.Text(int.to_string(model.counter), None, None),
      shore.DivRow([
        shore.Button("increment", key.Char("i"), Increment),
        shore.Button("decrement", key.Char("d"), Decrement),
      ]),
    ]),
    shore.Px(50),
    shore.Px(10),
  ))
}
