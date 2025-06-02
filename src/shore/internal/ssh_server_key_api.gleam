import gleam/bit_array
import gleam/erlang/atom
import gleam/erlang/charlist
import gleam/io
import gleam/result
import gleam/string

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

pub type CheckKey =
  List(fn(PublicUserKey, String) -> Bool)

pub fn is_auth_key(
  key: PublicUserKey,
  user: charlist.Charlist,
  opts: CheckKey,
) -> Bool {
  let assert [#(k2, _comment)] =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAII9JoJW9cu1+L5/m2uc7YPp7PB7tVGpUAZ4OWnaR36ZT bgw@bgw.dev"
    |> decode_ffi(OpensshKey)
    |> echo

  key == k2
}

@external(erlang, "ssh_file", "host_key")
fn host_key_ffi(
  algorithm: Algorithm,
  opts: DaemonOptions,
) -> Result(PrivateKey, Reason)

type SshKeyType {
  OpensshKey
}

pub type PublicUserKey =
  #(#(atom.Atom, BitArray, #(atom.Atom, #(Int, Int, Int, Int))))

pub type PublicUserKeyDecode =
  List(#(PublicUserKey, List(Comment)))

pub type Comment {
  Comment(BitArray)
}

@external(erlang, "ssh_file", "decode")
fn decode_ffi(key: String, type_: SshKeyType) -> PublicUserKeyDecode
