import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}
import gleam/io
import shore

pub type State {
  State(tty: TTY)
}

pub type State2 {
  State2(tty: TTY, pid: Pid)
}

pub type Reason

pub type Msg

pub type StartOpt

pub type Pid

pub type TTY

@external(erlang, "shore_ffi", "tty")
fn tty() -> TTY

@external(erlang, "shore_ffi", "ptyopts")
fn ptyopts() -> TODO

@external(erlang, "shore_ffi", "echo")
fn echo_ffi(tty: TODO) -> Nil

pub fn init(args: StartOpt) -> Result(State, Reason) {
  echo "hi"
  //let pty = ptyopts() |> echo
  echo "bye"
  //let tty = tty() |> echo
  echo "fry"
  //echo_ffi(tty)
  echo "init"
  Ok(State(tty: tty()))
}

pub type TODO

pub type SshChannelUp {
  SshChannelUp(i: Int, pid: Pid)
}

pub fn handle_msg(msg: SshChannelUp, state: State) -> Result(State2, TODO) {
  echo "handle_msg"
  echo msg
  //let chan = session_channel(msg.pid, 10) |> echo
  Ok(State2(state.tty, msg.pid))
}

pub fn handle_ssh_msg(msg: Msg, state: State2) -> Result(State2, TODO) {
  echo "handle_ssh_msg"
  echo msg
  send(state.pid, 0, shore.c(shore.Clear) <> shore.c(shore.MoveRight(5)) <> "x")
  io.println("hi")
  Ok(state)
}

pub fn terminate(reason: Reason, state: State) -> Nil {
  echo "terminate"
  Nil
}

pub type ChannelId

@external(erlang, "ssh_connection", "session_channel")
fn session_channel(ref: Pid, timeout: Int) -> Result(ChannelId, Reason)

@external(erlang, "ssh_connection", "send")
fn send(ref: Pid, id: Int, datae: String) -> Result(Nil, Reason)
