import gleam/erlang/charlist
import shore/internal/ssh_server

/// -behaviour(ssh_serve_key_api)
pub type Algorithm

pub type DaemonOptions

pub type PrivateKey

pub type Reason

/// Use default behavior of ssh_file
pub fn host_key(
  algorithm: Algorithm,
  opts: DaemonOptions,
) -> Result(PrivateKey, Reason) {
  host_key_ffi(algorithm, opts)
}

pub type DaemonKeyCbOption {
  KeyCbPrivate(ssh_server.CheckKey)
}

pub fn is_auth_key(
  key: ssh_server.PublicUserKeyFfi,
  user: charlist.Charlist,
  opts: List(DaemonKeyCbOption),
) -> Bool {
  case opts {
    [KeyCbPrivate([func]), ..] ->
      func(user |> charlist.to_string, ssh_server.PublicKey(key))
    _ ->
      panic as "framework bug, there should never be more than one function passed to is_auth_key"
  }
}

@external(erlang, "ssh_file", "host_key")
fn host_key_ffi(
  algorithm: Algorithm,
  opts: DaemonOptions,
) -> Result(PrivateKey, Reason)
