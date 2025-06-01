import gleam/erlang
import gleam/erlang/atom
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process
import shore/internal

// TODO: do we need user directory?
/// `system
pub type Config {
  Config(
    /// Path to directory with ssh_host keys
    /// https://www.erlang.org/doc/apps/ssh/ssh_file#SYSDIR
    system_directory: String,
    /// Path to directory with authorized_keys file
    /// https://www.erlang.org/doc/apps/ssh/ssh_file#USERDIR
    user_directory: String,
    /// TODO
    auth: Auth,
    /// port to expose the ssh server on
    port: Int,
  )
}

// TODO: explore auth api and options
pub type Auth {
  Anonymous
  Password(auth: fn(String, String) -> Bool)
  Key(auth: fn(String) -> Bool)
  KeyOrPassword(auth: fn(String, Secret) -> Bool)
}

fn config_auth(config: Auth) -> List(DaemonOption(model, msg)) {
  case config {
    Anonymous -> [NoAuthNeeded(True)]

    Password(app_auth) -> [
      NoAuthNeeded(False),
      AuthMethods("keyboard-interactive,password" |> charlist.from_string),
      Pwdfun(fn(user, secret) {
        let #(user, secret) = to_auth(user, secret)
        case secret {
          PublicKey(Nil) -> False
          UserPassword(password) -> app_auth(user, password)
        }
      }),
    ]

    Key(app_auth) -> [
      NoAuthNeeded(False),
      PkCheckUser,
      AuthMethods("publickey" |> charlist.from_string),
      Pwdfun(fn(user, secret) {
        let #(user, secret) = to_auth(user, secret)
        case secret {
          PublicKey(Nil) -> app_auth(user)
          UserPassword(..) -> False
        }
      }),
    ]

    KeyOrPassword(app_auth) -> [
      NoAuthNeeded(False),
      PkCheckUser,
      AuthMethods(
        "publickey,keyboard-interactive,password" |> charlist.from_string,
      ),
      Pwdfun(fn(user, secret) {
        let #(user, secret) = to_auth(user, secret)
        app_auth(user, secret)
      }),
    ]
  }
}

pub fn serve(
  spec: internal.Spec(model, msg),
  config: Config,
) -> Result(process.Pid, Nil) {
  let assert Ok(_) = erlang.ensure_all_started("ssh" |> atom.create_from_string)
  let opts = [
    SshCli(#("shore@internal@ssh_cli" |> atom.create_from_string, [spec])),
    SystemDir(config.system_directory |> charlist.from_string),
    UserDir(config.user_directory |> charlist.from_string),
    ..config_auth(config.auth)
  ]
  daemon_ffi(config.port, opts)
}

fn to_auth(user: Charlist, secret: SecretFfi) -> #(String, Secret) {
  let user = user |> charlist.to_string
  let secret = secret |> to_ssh_secret
  #(user, secret)
}

//
// FFI
//

type DaemonOption(model, msg) {
  SystemDir(Charlist)
  UserDir(Charlist)
  AuthMethods(Charlist)
  Pwdfun(fn(Charlist, SecretFfi) -> Bool)
  SshCli(#(atom.Atom, List(internal.Spec(model, msg))))
  NoAuthNeeded(Bool)
  PkCheckUser
}

@external(erlang, "ssh", "daemon")
fn daemon_ffi(
  port: Int,
  opts: List(DaemonOption(model, msg)),
) -> Result(process.Pid, Nil)

type SecretFfi

pub type Secret {
  PublicKey(Nil)
  UserPassword(String)
}

@external(erlang, "shore_ffi", "to_ssh_secret")
fn to_ssh_secret(secret: SecretFfi) -> Secret
