import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/io
import gleam/string
import shore
import shore/internal

pub type TODO

//
// API NOW
//

// TODO: there may be other things to handle here:
// https://www.erlang.org/doc/apps/ssh/ssh_client_channel.html#c:handle_msg/2
pub opaque type HandleMsg {
  SshChannelUp(channel_id: Int, pid: Pid)
}

// https://www.erlang.org/doc/apps/ssh/ssh_client_channel.html#c:handle_ssh_msg/2
pub opaque type HandleSshMsg {
  SshCm(pid: Pid, msg: ChannelMsg)
}

type ChannelMsg {
  Data(channel_id: Int, ssh_data_type_code: Int, data: String)
  Shell(a: Int, bool: Bool)
  Pty(
    channel_id: Int,
    want_reply: Bool,
    // TODO: map this to Terminal type
    terminal: #(Charlist, Int, Int, Int, Int, List(TODO)),
  )
  WindowChange(
    channel_id: Int,
    char_width: Int,
    row_height: Int,
    pixel_width: Int,
    pixel_height: Int,
  )
  // TODO: variants
  // eof
  // closed
  // env
  // shell
  // exec
  // signal
  // exit_status
  // exit_signal
}

type Terminal {
  Terminal(
    terminal: Charlist,
    char_width: Int,
    row_height: Int,
    pixel_width: Int,
    pixel_height: Int,
    // TODO: decide what to do with this info, do we need it?
    modes: List(TODO),
  )
}

// TODO: map to this?
//type SshDataTypeCode {
//  // 0
//  Normal
//  // 1
//  StdErr
//}

pub opaque type Reason {
  Normal
  Shutdown
  // TODO: #(shutdown, term())
  // TODO: term()
}

//
// START POINT
//

pub type Init(msg) {
  Init(shore: Subject(shore.Event(msg)))
}

pub type State(msg) {
  State(pid: Pid, shore: Subject(shore.Event(msg)))
}

pub type StartOpt

pub type TTY

@external(erlang, "shore_ffi", "tty")
fn tty() -> TTY

// TODO: error handling
pub fn init(args: List(internal.Spec(model, msg))) -> Continue(Init(msg)) {
  case args {
    [spec] -> {
      let assert Ok(shore) = spec |> shore.start as "init failed to start spec"
      Init(shore:) |> Ok |> to_continue
    }
    x -> panic as { "init was not expecting: " <> string.inspect(x) }
  }
}

pub fn handle_msg(msg: HandleMsg, state: Init(msg)) -> Continue(State(msg)) {
  echo "handle_msg"
  echo msg
  //let chan = session_channel(msg.pid, 10) |> echo
  State(msg.pid, state.shore) |> Ok |> to_continue
}

pub fn handle_ssh_msg(
  msg: HandleSshMsg,
  state: State(msg),
) -> Continue(State(msg)) {
  echo "handle_ssh_msg"
  echo msg
  case msg {
    SshCm(_, Data(data: "\u{0003}", ..)) -> {
      Error(Shutdown) |> to_continue
    }
    _x -> {
      let _ = send(state.pid, 0, "x")
      state |> Ok |> to_continue
    }
  }
}

// TODO: cleanup
// NOTE: return value is ignored
pub fn terminate(reason: Reason, state: State(msg)) -> Nil {
  echo "terminate"
  echo reason
  Nil
}

pub type ChannelId

@external(erlang, "ssh_connection", "session_channel")
fn session_channel(ref: Pid, timeout: Int) -> Result(ChannelId, Reason)

@external(erlang, "ssh_connection", "send")
fn send(ref: Pid, id: Int, datae: String) -> Result(Nil, Reason)

//
// HEPLERS
//

pub type Continue(state)

@external(erlang, "shore_ffi", "to_continue")
fn to_continue(result: Result(state, Reason)) -> Continue(state)
