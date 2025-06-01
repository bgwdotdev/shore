import shore/internal
import shore/internal/ssh_server

pub type Secret {
  PublicKey(Nil)
  UserPassword(String)
}

pub fn config(
  port port: Int,
  system_directory system_directory: String,
  user_directory user_directory: String,
  auth auth: ssh_server.Auth,
) -> ssh_server.Config {
  ssh_server.Config(port:, system_directory:, user_directory:, auth:)
}

/// Starts an ssh server which will provide the application to connecting clients
pub fn start(
  spec: internal.Spec(model, msg),
  config: ssh_server.Config,
) -> Result(Nil, Nil) {
  // TODO: fix return
  ssh_server.serve(spec, config)
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
/// fn user_login(username: String) -> Bool {
///   io.println("user logged in: " <> username)
///   True
/// }
///
/// fn main() {
///   let auth = auth_public_key(user_login)
///   config(auth:, ..)
/// }
/// ```
pub fn auth_public_key(auth: fn(String) -> Bool) -> ssh_server.Auth {
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
  auth: fn(String, Secret) -> Bool,
) -> ssh_server.Auth {
  fn(username: String, secret: ssh_server.Secret) -> Bool {
    let secret = case secret {
      ssh_server.PublicKey(Nil) -> PublicKey(Nil)
      ssh_server.UserPassword(password) -> UserPassword(password)
    }
    auth(username, secret)
  }
  |> ssh_server.KeyOrPassword
}
