import gleam/option.{type Option, None, Some}
import shore/internal.{type Node}
import shore/key.{type Key}
import shore/style

/// Sets alignment of all child nodes
pub fn align(alignment: style.Align, node: Node(msg)) -> Node(msg) {
  internal.Aligned(alignment, node)
}

/// A row with a background color
pub fn bar(color: style.Color) -> Node(msg) {
  internal.Bar(color)
}

/// A row with a background color, containing items
pub fn bar2(color: style.Color, node: Node(msg)) -> Node(msg) {
  internal.Bar2(color, node)
}

/// A box container element for holding other nodes
pub fn box(children: List(Node(msg)), title: Option(String)) -> Node(msg) {
  internal.Box(children, title)
}

/// An empty line
pub fn br() -> Node(msg) {
  internal.BR
}

/// A button assigned to a key press to execute an event
pub fn button(text: String, key: Key, event: msg) -> Node(msg) {
  internal.Button(text, key, event)
}

/// A container element for holding other nodes over multiple lines
pub fn col(children: List(Node(msg))) -> Node(msg) {
  internal.Col(children)
}

/// Prints some positional information for developer debugging
pub fn debug() -> Node(msg) {
  internal.Debug
}

/// An extremely simple plot
pub fn graph(
  width: style.Size,
  height: style.Size,
  points: List(Float),
) -> Node(msg) {
  internal.Graph(width, height, points)
}

/// A horizontal line
pub fn hr() -> Node(msg) {
  internal.HR
}

/// A colored horizontal line
pub fn hr_styled(color: style.Color) -> Node(msg) {
  internal.HR2(color)
}

/// A field for text input
pub fn input(
  label: String,
  value: String,
  width: style.Size,
  event: fn(String) -> msg,
) -> Node(msg) {
  internal.Input(label:, value:, width:, event:, submit: None, hidden: False)
}

/// A field for text input with the content display hidden, useful for password fields
pub fn input_hidden(
  label: String,
  value: String,
  width: style.Size,
  event: fn(String) -> msg,
) -> Node(msg) {
  internal.Input(label:, value:, width:, event:, submit: None, hidden: True)
}

/// A field for text input. Allows setting a `submit` event which can be
/// triggered by the submit keybind while the field is currently focused.
///
/// Useful for scenarios where a separate submit button would be inconvenient,
/// such as a chat box or 2fa prompt.
///
pub fn input_submit(
  label: String,
  value: String,
  width: style.Size,
  event: fn(String) -> msg,
  submit: msg,
  hidden: Bool,
) -> Node(msg) {
  internal.Input(label:, value:, width:, event:, submit: Some(submit), hidden:)
}

/// A non-visible button assigned to a key press to execute an event
pub fn keybind(key: Key, event: msg) -> Node(msg) {
  internal.KeyBind(key, event)
}

/// A progress bar, will automatically calculate fill percent based off max and current values
pub fn progress(
  width: style.Size,
  max: Int,
  value: Int,
  color: style.Color,
) -> Node(msg) {
  internal.Progress(width, max, value, color)
}

/// A container element for holding other nodes in a single line
pub fn row(children: List(Node(msg))) -> Node(msg) {
  internal.Row(children)
}

/// A table layout
pub fn table(width: style.Size, table: List(List(String))) -> Node(msg) {
  internal.Table(width, table)
}

/// A Key-Value style table layout
pub fn table_kv(width: style.Size, table: List(List(String))) -> Node(msg) {
  internal.TableKV(width, table)
}

/// A text string
pub fn text(text: String) -> Node(msg) {
  internal.TextMulti(text, None, None)
}

/// A text string with colored foreground and/or background
pub fn text_styled(
  text: String,
  fg: Option(style.Color),
  bg: Option(style.Color),
) -> Node(msg) {
  internal.TextMulti(text, fg, bg)
}
