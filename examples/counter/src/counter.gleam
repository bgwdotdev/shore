////shore.Px(50),

import gleam/erlang/process
import gleam/int
import gleam/option.{None, Some}
import shore
import shore/key
import shore/ui

// MAIN

pub fn main() {
  let exit = process.new_subject()
  let assert Ok(_actor) =
    shore.spec(
      init:,
      update:,
      view:,
      exit:,
      keybinds: shore.default_keybinds(),
      redraw: shore.on_timer(16),
    )
    |> shore.start
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
  ui.col([
    ui.text(
      "keybinds

i: increments
d: decrements
ctrl+x: exits
      ",
    ),
    ui.text(int.to_string(model.counter)),
    ui.row([
      ui.button("increment", key.Char("i"), Increment),
      ui.button("decrement", key.Char("d"), Decrement),
    ]),
  ])
}
