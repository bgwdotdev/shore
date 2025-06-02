import shore/internal
import shore/internal/ssh_server

/// An OpenSSH public key
pub type PublicKey =
  ssh_server.PublicKey

pub fn config(
  port port: Int,
  host_key_directory host_key_directory: String,
  auth auth: ssh_server.Auth,
) -> ssh_server.Config {
  ssh_server.Config(port:, host_key_directory:, auth:)
}

/// Starts an ssh server which will provide the application to connecting clients
pub fn start(
  spec: internal.Spec(model, msg),
  config: ssh_server.Config,
) -> Result(Nil, Nil) {
  // TODO: fix return
  let assert Ok(_) = ssh_server.serve(spec, config)
  Ok(Nil)
}

/// Allow anyone to connect without requiring a password or public key
pub fn auth_anonymous() -> ssh_server.Auth {
  ssh_server.Anonymous
}

/// Provide a password challenge for users to complete
///
/// ## Example
///
/// ```
/// fn user_login(username: String, password: String) -> Bool {
///   case username, password {
///     "Joe", "Hello!" -> True
///     _, _ -> False
///   }
/// }
///
/// fn main() {
///   let auth = auth_password(user_login)
///   config(auth:, ..)
/// }
/// ```
pub fn auth_password(auth: fn(String, String) -> Bool) -> ssh_server.Auth {
  ssh_server.Password(auth:)
}

/// Provide public key challenge. public key must be present in
/// `authorized_keys` file held in the `user_directory` folder.
///
/// Requires a callback function which containers the username on successful
/// authentication. Can be used for further validation, logging, etc.
///
/// Note: It is safe to have this function always return `True` e.g. `fn(_) { True }`
///
/// ## Example
///
/// ```
/// fn user_login(username: String, public_key: PublicKey) -> Bool {
///    let challenge =
///      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOE7rwqgX3K2Cj8wY/gAOiEQ0T9lEINdNwFq9HEVXB71 username@shore"
///      |> ssh.to_public_key
///    challenge == public_key && username == "Joe"
/// }
///
/// fn main() {
///   let auth = auth_public_key(user_login)
///   config(auth:, ..)
/// }
/// ```
pub fn auth_public_key(
  auth: fn(String, ssh_server.PublicKey) -> Bool,
) -> ssh_server.Auth {
  ssh_server.Key(auth)
}

/// Provide public key challenge, falling back to paswsord challenge if no
/// matching public key.
///
/// ## Example
///
/// ```
/// fn user_login(username: String, secret: Secret) -> Bool {
///   case username, secret {
///     "Joe", UserPassword("Hello!") -> True
///     _, PublicKey(Nil) -> True
///     _, _ -> False
///   }
/// }
///
/// fn main() {
///   let auth = auth_public_key_or_password(user_login)
///   config(auth:, ..)
/// }
/// ```
pub fn auth_public_key_or_password(
  password_auth password_auth: fn(String, String) -> Bool,
  key_auth key_auth: fn(String, PublicKey) -> Bool,
) -> ssh_server.Auth {
  ssh_server.KeyOrPassword(password_auth:, key_auth:)
}

/// Converts an OpenSSH public key string into a PublicKey type for comparison
///
/// ## Example
///
/// ```
/// "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOE7rwqgX3K2Cj8wY/gAOiEQ0T9lEINdNwFq9HEVXB71 username@shore"
/// |> to_public_key
/// ```
pub fn to_public_key(public_key: String) -> PublicKey {
  ssh_server.decode_key(public_key)
}
