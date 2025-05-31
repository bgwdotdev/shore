import gleam/erlang/charlist.{type Charlist}
import gleam/io

pub type State {
  State
}

pub type Reason

pub type Msg

pub type StartOpt

@external(erlang, "shore_ffi", "tty")
fn tty() -> TODO

@external(erlang, "os", "cmd")
fn cmd_ffi(cmd: Charlist) -> Charlist

@external(erlang, "shore_ffi", "echo")
fn echo_ffi(tty: TODO) -> Nil

pub fn init(args: StartOpt) -> Result(State, Reason) {
  let tty = tty() |> echo
  echo_ffi(tty)
  echo "init"
  Ok(State)
}

pub type TODO

pub fn handle_msg(msg: Msg, state: State) -> Result(State, TODO) {
  echo "handle_msg"
  echo msg
  Ok(state)
}

pub fn handle_ssh_msg(msg: Msg, state: State) -> Result(State, TODO) {
  echo "handle_ssh_msg"
  echo msg
  io.println("hi")
  Ok(state)
}

pub fn terminate(reason: Reason, state: State) -> Nil {
  echo "terminate"
  Nil
}
