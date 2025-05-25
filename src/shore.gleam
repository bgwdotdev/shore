import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import shore/internal
import shore/key.{type Key}

/// Send events to shore with the `send` function
pub type Event(msg) =
  internal.Event(msg)

/// Represents UI
pub type Node(msg) =
  internal.Node(msg)

/// A shore application is made up of these base parts. Following The Elm
/// Architecture, you must define an init, view and update function which shore
/// will handle calling.
///
/// Additionally, a simple subject to pass the exit call to is required.
/// keybinding for the framework level events such as exiting and ui focusing.
/// And finally redraw for defining when the applicaiton should be redrawn, either on update messages or on a timer.
///
/// ## Example
/// ```
/// import gleam/erlang/process
/// import shore
///
/// pub fn main() {
///   let exit = process.new_subject()
///   let assert Ok(_actor) =
///     shore.spec(
///       init:,
///       update:,
///       view:,
///       exit:,
///       keybinds: shore.default_keybinds(),
///       redraw: shore.on_timer(16),
///     )
///     |> shore.start
///   exit |> process.receive_forever
/// }
///
/// ```
///
pub fn spec(
  init init: fn() -> #(model, List(fn() -> msg)),
  view view: fn(model) -> internal.Node(msg),
  update update: fn(model, msg) -> #(model, List(fn() -> msg)),
  exit exit: Subject(Nil),
  keybinds keybinds: internal.Keybinds,
  redraw redraw: internal.Redraw,
) -> internal.Spec(model, msg) {
  internal.Spec(init:, view:, update:, exit:, keybinds:, redraw:)
}

/// Starts the application actor and returns its subject
pub fn start(
  spec: internal.Spec(model, msg),
) -> Result(Subject(Event(msg)), actor.StartError) {
  internal.start(spec)
}

/// Set keybinds for various shore level functions, such as moving between
/// focusable elements such as input boxes and buttons, as well as exiting and
/// triggering button events.
pub fn keybinds(
  exit exit: Key,
  submit submit: Key,
  focus_clear focus_clear: Key,
  focus_next focus_next: Key,
  focus_prev focus_prev: Key,
) -> internal.Keybinds {
  internal.Keybinds(exit:, submit:, focus_clear:, focus_next:, focus_prev:)
}

/// A typical set of keybindings
///
/// - exit: `ctrl+x`
/// - submit: `enter`
/// - focus_clear: `escape`
/// - focus_next: `tab`
/// - focus_prv: `shift+tab`
pub fn default_keybinds() -> internal.Keybinds {
  internal.Keybinds(
    exit: key.Ctrl("X"),
    submit: key.Enter,
    focus_clear: key.Esc,
    focus_next: key.Tab,
    focus_prev: key.BackTab,
  )
}

/// Allows sending a message to your TUI from another actor. This can be used,
/// for example, to push an event to your TUI, rather than have it poll.
///
pub fn send(msg: msg) -> internal.Event(msg) {
  internal.send(msg)
}

/// Manually trigger the exit for your TUI. Normally this would be handled
/// through the exit keybind.
///
pub fn exit() -> Event(msg) {
  internal.exit()
}

/// Redraw every x milliseconds
pub fn on_timer(ms ms: Int) -> internal.Redraw {
  internal.OnTimer(ms:)
}

/// Redraw in response to events. Suitable for infrequently changing state.
pub fn on_update() -> internal.Redraw {
  internal.OnUpdate
}
