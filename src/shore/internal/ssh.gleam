import gleam/erlang
import gleam/erlang/atom
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process
import gleam/io
import shore/internal

// TODO: do we need user directory?
pub type Config {
  Config(host_key_directory: String, auth: Auth, user_directory: String)
}

// TODO: explore auth api and options
pub type Auth {
  Anonymous
  Password
  Keys
}

// TODO: spec configs: one-to-one vs one-to-many?
pub fn serve(spec: internal.Spec(model, msg)) -> Result(process.Pid, Nil) {
  let assert Ok(_) = erlang.ensure_all_started("ssh" |> atom.create_from_string)
  [
    SystemDir("." |> charlist.from_string),
    UserDir("/home/bgw/.ssh" |> charlist.from_string),
    //Pwdfun(auth),
    NoAuthNeeded(True),
    SshCli(#("shore@sshcli" |> atom.create_from_string, [spec])),
  ]
  |> daemon_ffi(2222, _)
}

fn auth(user: Charlist, secret: Charlist) -> Bool {
  True
}

//
// FFI
//

type DaemonOption(model, msg) {
  SystemDir(Charlist)
  UserDir(Charlist)
  AuthMethods(Charlist)
  Pwdfun(fn(Charlist, Charlist) -> Bool)
  SshCli(#(atom.Atom, List(internal.Spec(model, msg))))
  NoAuthNeeded(Bool)
}

@external(erlang, "ssh", "daemon")
fn daemon_ffi(
  port: Int,
  opts: List(DaemonOption(model, msg)),
) -> Result(process.Pid, Nil)
