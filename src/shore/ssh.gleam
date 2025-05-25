import gleam/erlang
import gleam/erlang/atom
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process
import gleam/io
import shore

pub fn serve(spec: shore.Spec(model, msg)) -> Result(process.Pid, Nil) {
  let assert Ok(_) = erlang.ensure_all_started("ssh" |> atom.create_from_string)
  [
    SystemDir("." |> charlist.from_string),
    UserDir("/home/bgw/.ssh" |> charlist.from_string),
    NoAuthNeeded(True),
    Shell(fn(user, peer) { shell(user, peer, spec) }),
  ]
  |> daemon_ffi(2222, _)
}

fn auth(user: Charlist, secret: Charlist) -> Bool {
  True
}

fn shell(
  user: Charlist,
  peer: Peer,
  spec: shore.Spec(model, msg),
) -> process.Pid {
  raw_erl()
  let pid =
    process.start(
      fn() {
        let subj = process.new_subject()
        let assert Ok(Nil) =
          subj
          |> process.subject_owner()
          |> process.register("shoressh" |> atom.create_from_string)
        //spec |> shore.start
        named_loop(subj)
      },
      False,
    )

  process.start(
    fn() { get_chars("", 10) |> charlist.to_string |> io.print },
    True,
  )
  pid
}

@external(erlang, "shore_ffi", "thing")
fn thing(fun: fn(String) -> Nil) -> Nil

fn named_loop(subject: process.Subject(String)) -> Nil {
  thing(fn(str) { str |> io.print })
  //case process.receive_forever(subject) {
  //  x -> x |> io.print
  //}
  named_loop(subject)
}

fn test_loop(subject: process.Subject(SshChannelMsg)) -> Nil {
  case get_line("write something: ") |> charlist.to_string {
    "hi\n" -> "bye" |> io.println
    x -> { "got input: " <> x } |> io.println
  }
  test_loop(subject)
  //case process.receive_forever(subject) {
  //  Data(_, _, _) -> io.println("hi")
  //  Eof(_) -> io.println("bye")
  //  x -> {
  //    x |> echo
  //    io.println("something")
  //  }
  //}
}

//
// FFI
//

type DaemonOption {
  SystemDir(Charlist)
  UserDir(Charlist)
  AuthMethods(Charlist)
  Pwdfun(fn(Charlist, Charlist) -> Bool)
  Shell(fn(Charlist, Peer) -> process.Pid)
  NoAuthNeeded(Bool)
}

type SshConnection {
  SshConnection(ssh: Ssh, channel: Channel, peer: Peer, pty: Pty)
}

type Ssh

type Channel

type Peer =
  #(#(Int, Int, Int, Int), Int)

type Pty

type TODO

type SshEvent {
  SshCm(connection_ref: process.Pid, data: SshChannelMsg)
}

// TODO: other events: https://www.erlang.org/doc/apps/ssh/ssh_connection.html#t:channel_msg/0
type SshChannelMsg {
  Data(channel: Channel, data_type_code: Int, data: TODO)
  Eof(channel: Channel)
}

@external(erlang, "ssh", "daemon")
fn daemon_ffi(port: Int, opts: List(DaemonOption)) -> Result(process.Pid, Nil)

@external(erlang, "ssh_connection", "send")
fn send_ffi(channel: Channel, msg: Charlist) -> Nil

@external(erlang, "io", "get_chars")
fn get_chars(prompt: String, count: Int) -> Charlist

@external(erlang, "io", "get_line")
fn get_line(prompt: String) -> Charlist

type ShellOpt {
  Noshell
  Raw
}

fn raw_erl() {
  raw_ffi(#(Noshell, Raw))
}

type DoNotLeak

@external(erlang, "shell", "start_interactive")
fn raw_ffi(opts: #(ShellOpt, ShellOpt)) -> DoNotLeak
