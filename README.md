# Shore

[![Package Version](https://img.shields.io/hexpm/v/shore)](https://hex.pm/packages/shore)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/shore/)

Shore is a TUI (terminal user interface) framework following The Elm Architecture for Gleam.

It is purely focused on the erlang runtime.

Shore requires erlang/otp 28 at minimum.

Further documentation can be found at <https://hexdocs.pm/shore>.

## Example

Check out the `examples/` directory for more information and showcase of features such as commands, layout and styling

```sh
gleam add shore@1
```

```gleam
import gleam/erlang/process
import gleam/int
import gleam/option.{None}
import shore
import shore/key

// MAIN

pub fn main() {
  let exit = process.new_subject()
  let assert Ok(_actor) =
    shore.Spec(
      init:,
      update:,
      view:,
      exit:,
      keybinds: shore.default_keybinds(),
      redraw: shore.OnUpdate,
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
  shore.Div(
    [
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
      shore.Div(
        [
          shore.Button("increment", key.Char("i"), Increment),
          shore.Button("decrement", key.Char("d"), Decrement),
        ],
        shore.Row,
      ),
    ],
    shore.Col,
  )
}
```


