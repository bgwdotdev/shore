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
  internal.Box(children, title, None)
}

/// A box container element for holding other nodes.
///
/// Can be provided with a custom colour for the outline and title.
pub fn box_styled(
  children: List(Node(msg)),
  title: Option(String),
  fg: Option(style.Color),
) -> Node(msg) {
  internal.Box(children, title, fg)
}

/// An empty line
pub fn br() -> Node(msg) {
  internal.BR
}

/// A button assigned to a key press to execute an event
pub fn button(text: String, key: Key, event: msg) -> Node(msg) {
  internal.Button(
    id: text,
    text:,
    key:,
    event:,
    fg: Some(style.Black),
    bg: Some(style.Blue),
    focus_fg: Some(style.Black),
    focus_bg: Some(style.Green),
  )
}

/// A button assigned to a key press to execute an event.
/// Can be provided with custom colours both for when focused/pressed or not.
///
/// Default colors for buttons are:
/// ```gleam
///  fg: Some(style.Black),
///  bg: Some(style.Blue),
///  focus_fg: Some(style.Black),
///  focus_bg: Some(style.Green),
/// ```
///
pub fn button_styled(
  text: String,
  key: Key,
  event: msg,
  fg: Option(style.Color),
  bg: Option(style.Color),
  focus_fg: Option(style.Color),
  focus_bg: Option(style.Color),
) -> Node(msg) {
  internal.Button(id: text, text:, key:, event:, fg:, bg:, focus_fg:, focus_bg:)
}

/// A button assigned to a key press to execute an event
///
/// Takes an `id` value which uniquely identifies it, allowing two buttons to
/// share the same display text but operate independently, contrary to a
/// button, where the text is the id and so all button text must be unique.
pub fn button_id(id: String, text: String, key: Key, event: msg) -> Node(msg) {
  internal.Button(
    id:,
    text:,
    key:,
    event:,
    fg: Some(style.Black),
    bg: Some(style.Blue),
    focus_fg: Some(style.Black),
    focus_bg: Some(style.Green),
  )
}

/// A button assigned to a key press to execute an event.
///
/// Takes an `id` value which uniquely identifies it, allowing two buttons to
/// share the same display text but operate independently, contrary to a
/// button, where the text is the id and so all button text must be unique.
///
/// Can be provided with custom colours both for when focused/pressed or not.
///
/// Default colors for buttons are:
/// ```gleam
///  fg: Some(style.Black),
///  bg: Some(style.Blue),
///  focus_fg: Some(style.Black),
///  focus_bg: Some(style.Green),
/// ```
///
pub fn button_id_styled(
  id: String,
  text: String,
  key: Key,
  event: msg,
  fg: Option(style.Color),
  bg: Option(style.Color),
  focus_fg: Option(style.Color),
  focus_bg: Option(style.Color),
) -> Node(msg) {
  internal.Button(id:, text:, key:, event:, fg:, bg:, focus_fg:, focus_bg:)
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
  internal.Input(
    label:,
    value:,
    width:,
    event:,
    submit: None,
    hidden: False,
    focus_on: key.Null,
  )
}

/// A field for text input with the content display hidden, useful for password fields
pub fn input_hidden(
  label: String,
  value: String,
  width: style.Size,
  event: fn(String) -> msg,
) -> Node(msg) {
  internal.Input(
    label:,
    value:,
    width:,
    event:,
    submit: None,
    hidden: True,
    focus_on: key.Null,
  )
}

/// A field for text input.
///
/// Allows setting a `submit` event which can be triggered by the submit
/// keybind while the field is currently focused.
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
  internal.Input(
    label:,
    value:,
    width:,
    event:,
    submit: Some(submit),
    hidden:,
    focus_on: key.Null,
  )
}

/// A field for text input.
///
/// Allows setting a keybind to set focus directly to the field.
///
/// Useful for scenarios such as jumping directly to a search bar.
///
pub fn input_keybind(
  label: String,
  value: String,
  width: style.Size,
  event: fn(String) -> msg,
  keybind: Key,
) -> Node(msg) {
  internal.Input(
    label:,
    value:,
    width:,
    event:,
    submit: None,
    hidden: False,
    focus_on: keybind,
  )
}

/// A field for text input.
///
/// Allows setting a keybind to set focus directly to the field.
///
/// Allows setting a `submit` event which can be triggered by the submit
/// keybind while the field is currently focused.
///
/// Useful for scenarios such as jumping directly to a search bar and
/// triggering an expensive search (e.g. via an API call), upon pressing enter.
///
pub fn input_keybind_submit(
  label: String,
  value: String,
  width: style.Size,
  event: fn(String) -> msg,
  submit: msg,
  hidden: Bool,
  keybind: Key,
) -> Node(msg) {
  internal.Input(
    label:,
    value:,
    width:,
    event:,
    submit: Some(submit),
    hidden:,
    focus_on: keybind,
  )
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
@deprecated("use paragaph instead")
pub fn text(text: String) -> Node(msg) {
  internal.TextMulti(text, internal.NoWrap, None, None, [])
}

pub fn paragraph(text: String) -> Node(msg) {
  internal.TextMulti(text, internal.NoWrap, None, None, [])
}

/// A text string with automatic line wrapping
@deprecated("use text_new builder instead")
pub fn text_wrapped(text: String) -> Node(msg) {
  internal.TextMulti(text, internal.Wrap, None, None, [])
}

/// A text string with colored foreground and/or background
@deprecated("use text_new builder instead")
pub fn text_styled(
  text: String,
  fg: Option(style.Color),
  bg: Option(style.Color),
) -> Node(msg) {
  internal.TextMulti(text, internal.NoWrap, fg, bg, [])
}

/// A text string with automatic line wrapping and colored foreground and/or background
@deprecated("use text_new builder instead")
pub fn text_wrapped_styled(
  text: String,
  fg: Option(style.Color),
  bg: Option(style.Color),
) -> Node(msg) {
  internal.TextMulti(text, internal.Wrap, fg, bg, [])
}

/// Buildable text element, see `text_new` for usage
pub opaque type Text {
  Text(
    content: String,
    foreground: Option(style.Color),
    background: Option(style.Color),
    graphics: List(style.Graphic),
    wrap: internal.TextWrap,
  )
}

/// A text builder to create text elements with various styles applied
///
///
/// ```gleam
/// "my paragaph of text"
/// |> ui.text_new
/// |> ui.text_foreground(style.Black)
/// |> ui.text_background(style.Cyan)
/// |> ui.text_graphic(style.Bold)
/// |> ui.text_graphic(style.Italic)
/// |> ui.text_to_paragraph
/// ```
///
pub fn text_new(content: String) -> Text {
  Text(
    content: content,
    foreground: None,
    background: None,
    graphics: [],
    wrap: internal.NoWrap,
  )
}

/// sets the text foreground color
pub fn text_foreground(text: Text, color: style.Color) -> Text {
  Text(..text, foreground: Some(color))
}

/// sets the text background color
pub fn text_background(text: Text, color: style.Color) -> Text {
  Text(..text, background: Some(color))
}

/// applies SGR style to text, such as bold, underline, etc (see style.Graphic for compelete list)
/// can be called multiple times
pub fn text_graphic(text: Text, graphic: style.Graphic) -> Text {
  Text(..text, graphics: [graphic, ..text.graphics])
}

/// sets the text to automatically line wrap
pub fn text_wrap(text: Text) -> Text {
  Text(..text, wrap: internal.Wrap)
}

/// builds the text item into a shore ui node
pub fn text_to_paragraph(text: Text) -> Node(msg) {
  internal.TextMulti(
    text.content,
    text.wrap,
    text.foreground,
    text.background,
    text.graphics,
  )
}

/// UNSTABLE: A base64 encoded png image drawn using the Kitty Graphics Protocol
///
/// This is currently a unstable implementation/exploration of using the kitty graphics protocol, some general notes are:
/// - visually large pngs and file sizes over 500kb~ have performance issues
/// - only pngs stored as a base64 string are supported
/// - only some terminals support this protocol
/// - this function is likely to change significantly as the implementation of image support is refined
/// - expect bugs
///
pub fn image_unstable(
  base64: String,
  width: style.Size,
  height: style.Size,
) -> Node(msg) {
  internal.KittyGraphic(base64, width, height)
}
