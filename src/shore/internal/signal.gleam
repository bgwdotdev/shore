import gleam/erlang/atom.{type Atom}
import gleam/erlang/process

const module = "shore@internal@signal"

pub fn start(state: fn() -> Nil) -> Nil {
  set_signal(Sigwinch, Handle)
  add_handler(atom.create("erl_signal_server"), atom.create(module), state)
  process.sleep_forever()
}

pub type State =
  fn() -> Nil

pub opaque type Signal {
  Sigwinch
}

pub fn init(state: State) -> Result(State, Nil) {
  Ok(state)
}

pub fn handle_event(signal: Signal, state: State) -> Result(State, Nil) {
  case signal {
    Sigwinch -> {
      state()
      Ok(state)
    }
  }
}

type Option {
  Handle
}

@external(erlang, "os", "set_signal")
fn set_signal(signal: Signal, option: Option) -> Nil

@external(erlang, "gen_event", "add_handler")
fn add_handler(event_manager_ref: Atom, handler: Atom, args: State) -> Nil
