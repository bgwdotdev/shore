@target(erlang)
import gleam/erlang/process
import gleam/int
import gleam/option.{Some}
import shore
import shore/effect.{type Effect}
import shore/key
import shore/layout
import shore/style
import shore/ui

// MAIN

@target(erlang)
pub fn main() {
  let exit_subject = process.new_subject()
  let exit = fn(nil) { process.send(exit_subject, nil) }
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
  exit_subject |> process.receive_forever
}

@target(javascript)
pub fn main() {
  let exit = fn(_) { Nil }
  let assert Ok(_actor) =
    shore.spec(
      init:,
      update:,
      view:,
      exit:,
      keybinds: shore.default_keybinds(),
      redraw: shore.on_update(),
    )
    |> shore.start
  Nil
}

// MODEL

type Model {
  Model(counter: Int)
}

fn init() -> #(Model, Effect(Msg)) {
  let model = Model(counter: 0)
  let effect = effect.none()
  #(model, effect)
}

// UPDATE

type Msg {
  Increment
  Decrement
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    Increment -> #(Model(counter: model.counter + 1), effect.none())
    Decrement -> #(Model(counter: model.counter - 1), effect.none())
  }
}

// VIEW

fn view(model: Model) -> shore.Node(Msg) {
  ui.box(
    [
      ui.paragraph(
        "keybinds

i: increments
d: decrements
ctrl+x: exits
      ",
      ),
      ui.paragraph(int.to_string(model.counter)),
      ui.br(),
      ui.row([
        ui.button("increment", key.Char("i"), Increment),
        ui.button("decrement", key.Char("d"), Decrement),
      ]),
    ],
    Some("counter"),
  )
  |> ui.align(style.Center, _)
  |> layout.center(style.Px(50), style.Px(12))
}
