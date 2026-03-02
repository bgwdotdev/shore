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

/// handler for erl_signal_server must handle all events listed
/// https://www.erlang.org/doc/apps/kernel/kernel_app.html#events
///
pub type Signal {
  Sighup
  Sigquit
  Sigabrt
  Sigalrm
  Sigterm
  Sigusr1
  Sigusr2
  Sigchld
  Sigstop
  Sigtstp
  Sigcont
  Sigwinch
  Siginfo
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
    _ -> Ok(state)
  }
}

type Option {
  Handle
}

@external(erlang, "os", "set_signal")
fn set_signal(signal: Signal, option: Option) -> Nil

@external(erlang, "gen_event", "add_handler")
fn add_handler(event_manager_ref: Atom, handler: Atom, args: State) -> Nil
