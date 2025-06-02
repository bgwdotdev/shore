import gleam/erlang
import gleam/erlang/atom
import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process
import shore/internal

// TODO: do we need user directory?
/// `system
pub type Config {
  Config(
    /// port to expose the ssh server on
    port: Int,
    /// Path to directory with ssh_host keys
    /// https://www.erlang.org/doc/apps/ssh/ssh_file#SYSDIR
    host_key_directory: String,
    /// TODO
    auth: Auth,
  )
}

// TODO: explore auth api and options
pub type Auth {
  Anonymous
  Password(auth: fn(String, String) -> Bool)
  Key(auth: fn(String, PublicKey) -> Bool)
  KeyOrPassword(
    password_auth: fn(String, String) -> Bool,
    key_auth: fn(String, PublicKey) -> Bool,
  )
}

fn config_auth(config: Auth) -> List(DaemonOption(model, msg)) {
  case config {
    Anonymous -> [NoAuthNeeded(True)]

    Password(app_auth) -> [
      NoAuthNeeded(False),
      AuthMethods("keyboard-interactive,password" |> charlist.from_string),
      Pwdfun(fn(user, secret, _peer_address, state) {
        let #(user, secret) = to_auth(user, secret)
        let ok = app_auth(user, secret)
        throttle(ok, state)
      }),
    ]

    Key(app_auth) -> [
      NoAuthNeeded(False),
      AuthMethods("publickey" |> charlist.from_string),
      KeyCb(
        #("shore@internal@ssh_server_key_api" |> atom.create_from_string, [
          app_auth,
        ]),
      ),
    ]

    KeyOrPassword(password_auth:, key_auth:) -> [
      NoAuthNeeded(False),
      AuthMethods(
        "publickey,keyboard-interactive,password" |> charlist.from_string,
      ),
      Pwdfun(fn(user, secret, _peer_address, state) {
        let #(user, secret) = to_auth(user, secret)
        let ok = password_auth(user, secret)
        throttle(ok, state)
      }),
      KeyCb(
        #("shore@internal@ssh_server_key_api" |> atom.create_from_string, [
          key_auth,
        ]),
      ),
    ]
  }
}

pub type AuthState {
  Undefined
  AuthState(throttle: Int)
}

fn throttle(ok: Bool, state: AuthState) -> #(Bool, AuthState) {
  case ok, state {
    True, _ -> #(True, Undefined)
    False, Undefined -> {
      process.sleep(1000)
      #(False, AuthState(throttle: 2000))
    }
    False, AuthState(throttle:) -> {
      process.sleep(throttle)
      #(False, AuthState(throttle: throttle * 2))
    }
  }
}

pub fn serve(
  spec: internal.Spec(model, msg),
  config: Config,
) -> Result(process.Pid, Nil) {
  let assert Ok(_) = erlang.ensure_all_started("ssh" |> atom.create_from_string)
  let opts = [
    SshCli(#("shore@internal@ssh_cli" |> atom.create_from_string, [spec])),
    SystemDir(config.host_key_directory |> charlist.from_string),
    Shell(Disabled),
    Exec(Disabled),
    ParallelLogin(True),
    ..config_auth(config.auth)
  ]
  daemon_ffi(config.port, opts)
}

fn to_auth(user: Charlist, secret: Charlist) -> #(String, String) {
  let user = user |> charlist.to_string
  let secret = secret |> charlist.to_string
  #(user, secret)
}

pub type PublicKey {
  PublicKey(key: PublicUserKeyFfi)
}

//
// FFI
//

type PeerAddress =
  #(#(Int, Int, Int, Int), Int)

pub type PublicUserKeyFfi =
  #(#(atom.Atom, BitArray, #(atom.Atom, #(Int, Int, Int, Int))))

pub type CheckKey =
  List(fn(String, PublicKey) -> Bool)

pub type DaemonOption(model, msg) {
  SystemDir(Charlist)
  AuthMethods(Charlist)
  Pwdfun(fn(Charlist, Charlist, PeerAddress, AuthState) -> #(Bool, AuthState))
  SshCli(#(atom.Atom, List(internal.Spec(model, msg))))
  NoAuthNeeded(Bool)
  Exec(Disabled)
  Shell(Disabled)
  ParallelLogin(Bool)
  KeyCb(#(atom.Atom, CheckKey))
}

@external(erlang, "ssh", "daemon")
fn daemon_ffi(
  port: Int,
  opts: List(DaemonOption(model, msg)),
) -> Result(process.Pid, Nil)

pub type Disabled {
  Disabled
}

type SshKeyType {
  OpensshKey
}

pub type PublicUserKeyDecode =
  List(#(PublicUserKeyFfi, List(Comment)))

pub type Comment {
  Comment(BitArray)
}

@external(erlang, "ssh_file", "decode")
fn decode_ffi(key: String, type_: SshKeyType) -> PublicUserKeyDecode

pub fn decode_key(public_key: String) -> PublicKey {
  // TODO: we probably shouldn't assert here, what does ffi do, just panic also?
  let assert [#(key, _comment)] = public_key |> decode_ffi(OpensshKey)
  PublicKey(key)
}
