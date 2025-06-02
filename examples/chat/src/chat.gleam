import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import shore
import shore/key
import shore/layout
import shore/ssh
import shore/style
import shore/ui

// MAIN

pub fn main() {
  let exit = process.new_subject()
  let spec =
    shore.spec(
      init:,
      update:,
      view:,
      exit:,
      keybinds: shore.default_keybinds(),
      redraw: shore.on_timer(16),
    )
  let config =
    ssh.config(
      port: 2222,
      host_key_directory: ".",
      auth: ssh.auth_public_key(fn(username, public_key) {
        let challenge =
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII9JoJW9cu1+L5/m2uc7YPp7PB7tVGpUAZ4OWnaR36ZT bgw@bgw.dev"
          |> ssh.to_public_key
        challenge == public_key && username == "bgw"
      }),
    )
  let assert Ok(_) = ssh.start(spec, config)
  process.sleep_forever()
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
  layout.center(
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
    ]),
    style.Px(50),
    style.Px(10),
  )
}
