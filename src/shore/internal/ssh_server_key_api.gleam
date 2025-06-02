import gleam/bit_array
import gleam/erlang/atom
import gleam/erlang/charlist
import gleam/io
import gleam/result
import gleam/string

/// -behaviour(ssh_serve_key_api)
pub type Algorithm

// 'ecdsa-sha2-nistp256' 
// 'ecdsa-sha2-nistp384' 
// 'ecdsa-sha2-nistp521'
// 'ssh-ed25519' 
// 'ssh-ed448'
// 'rsa-sha2-256' 
// 'rsa-sha2-512' 
// 'ssh-dss' 
// 'ssh-rsa'

pub type DaemonOptions

pub type PrivateKey

pub type Reason

pub type A

pub type B

@external(erlang, "shore_ffi", "host_key")
fn read_host_key() -> Result(List(#(A, String, B)), Reason)

pub fn host_key(
  algorithm: Algorithm,
  opts: DaemonOptions,
) -> Result(PrivateKey, Reason) {
  host_key_ffi(algorithm, opts)
}

pub type PublicUserKey =
  #(#(atom.Atom, BitArray, #(atom.Atom, #(Int, Int, Int, Int))))

pub type PublicUserKeyDecode =
  List(#(PublicUserKey, List(Comment)))

pub type Comment {
  Comment(BitArray)
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

fn parse_key(key: String) -> Result(String, Nil) {
  case key |> string.trim |> string.split(on: " ") {
    [algo, key, _comment] -> Ok(key)
    [algo, key] -> Ok(key)
    _ -> Error(Nil)
  }
}

@external(erlang, "file", "read_file")
fn read_file_ffi(path: String) -> Result(String, Reason)

@external(erlang, "public_key", "pem_decode")
fn pem_decode_ffi(key: String) -> #(A, String)

@external(erlang, "ssh_file", "host_key")
fn host_key_ffi(
  algorithm: Algorithm,
  opts: DaemonOptions,
) -> Result(PrivateKey, Reason)

@external(erlang, "ssh_file", "is_auth_key")
fn is_auth_key_ffi(
  key: PublicUserKey,
  user: charlist.Charlist,
  opts: CheckKey,
) -> Bool

type SshKeyType {
  OpensshKey
}

@external(erlang, "ssh_file", "decode")
fn decode_ffi(key: String, type_: SshKeyType) -> PublicUserKeyDecode

@external(erlang, "ssh_file", "encode")
fn encode_ffi(key: PublicUserKey, type_: SshKeyType) -> BitArray

@external(erlang, "ssh_message", "ssh2_pubkey_encode")
fn ssh2_pubkey_encode_ffi(key: PublicUserKey) -> BitArray

type SshAlgorithm

@external(erlang, "ssh_transport", "public_algo")
fn public_algo_ffi(key: PublicUserKey) -> SshAlgorithm
