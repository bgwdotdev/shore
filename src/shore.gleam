import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/pair
import gleam/result
import gleam/string
import shore/key.{type Key}

//
// INIT
//

pub type Spec(model, msg) {
  Spec(
    init: fn() -> #(model, List(fn() -> msg)),
    view: fn(model) -> Node(msg),
    update: fn(model, msg) -> #(model, List(fn() -> msg)),
    exit: process.Subject(Nil),
    keybinds: Keybinds,
  )
}

type State(model, msg) {
  State(
    spec: Spec(model, msg),
    model: model,
    tasks: process.Subject(Event(msg)),
    mode: Mode,
    last_input: String,
    focused: Option(Focused(msg)),
  )
}

pub type Keybinds {
  Keybinds(
    exit: Key,
    focus_clear: Key,
    focus_next: Key,
    focus_prev: Key,
    mode_insert: Key,
    mode_normal: Key,
  )
}

pub fn default_keybinds() -> Keybinds {
  Keybinds(
    exit: key.Char("Q"),
    focus_clear: key.Esc,
    focus_next: key.Char("J"),
    focus_prev: key.Char("K"),
    mode_insert: key.Char("I"),
    mode_normal: key.Esc,
  )
}

type Focused(msg) {
  Focused(
    label: String,
    value: String,
    event: fn(String) -> msg,
    cursor: Int,
    offset: Int,
    width: Int,
  )
}

type Mode {
  Insert
  Normal
}

pub fn start(spec: Spec(model, msg)) -> Subject(Event(msg)) {
  raw_erl()
  // TODO: set this and fix exit
  { c(HideCursor) <> c(AltBuffer) } |> io.print
  let assert Ok(shore) = shore_start(spec)
  process.start(fn() { read_input(shore, spec.keybinds.exit) }, False)
  shore
}

fn shore_start(spec: Spec(model, msg)) {
  actor.Spec(
    init: fn() {
      let tasks = process.new_subject()
      let #(model, task_init) = spec.init()
      let state =
        State(
          spec:,
          model:,
          tasks:,
          mode: Normal,
          last_input: "",
          focused: None,
        )
      let _first_paint = model |> spec.view |> render(state, _, key.Char(""))
      let queue =
        process.new_selector() |> process.selecting(tasks, function.identity)
      task_init |> task_handler(tasks)
      actor.Ready(state, queue)
    },
    init_timeout: 1000,
    loop: shore_loop,
  )
  |> actor.start_spec
}

//
// EVENT
//

pub opaque type Event(msg) {
  KeyPress(Key)
  Cmd(msg)
  Exit
}

pub fn cmd(msg: msg) -> Event(msg) {
  Cmd(msg)
}

pub fn exit() -> Event(msg) {
  Exit
}

fn shore_loop(event: Event(msg), state: State(model, msg)) {
  case event {
    Cmd(msg) -> {
      let #(model, tasks) = state.spec.update(state.model, msg)
      tasks |> task_handler(state.tasks)
      let state = State(..state, model: model)
      state.model |> state.spec.view |> render(state, _, key.Char(""))
      actor.continue(state)
    }
    KeyPress(input) -> {
      case state.mode {
        Normal -> {
          let ui = state.spec.view(state.model)
          let state = case control_event(input, state.spec.keybinds) {
            // TODO: decouple mode from focus clear?
            FocusClear -> State(..state, focused: None, mode: Normal)
            FocusPrev -> {
              let focusable = list_focusable([ui], [])
              let focused = case state.focused {
                None -> focusable |> list.first |> option.from_result
                Some(x) -> focusable |> focus_next(x, False)
              }
              State(..state, focused:)
            }
            FocusNext -> {
              let focusable = list_focusable([ui], []) |> list.reverse
              let focused = case state.focused {
                None -> focusable |> list.first |> option.from_result
                Some(x) -> focusable |> focus_next(x, False)
              }
              State(..state, focused:)
            }
            ModeInsert -> State(..state, mode: Insert)
            PassInput(input) -> {
              let model = case detect_event(state, ui, input) {
                Some(msg) -> {
                  let #(model, tasks) = state.spec.update(state.model, msg)
                  tasks |> task_handler(state.tasks)
                  model
                }
                None -> state.model
              }
              State(..state, model:)
            }
          }
          state.model |> state.spec.view |> render(state, _, input)
          actor.continue(state)
        }
        Insert -> {
          let mode = case input == state.spec.keybinds.mode_normal {
            True -> Normal
            False -> Insert
          }
          let #(focused, model) = case state.focused {
            Some(focused) -> {
              let focused = input_handler(focused, input)
              let #(model, tasks) =
                state.spec.update(state.model, focused.event(focused.value))
              tasks |> task_handler(state.tasks)
              #(Some(focused), model)
            }
            x -> #(x, state.model)
          }
          let state = State(..state, focused:, mode:, model:)
          state.model |> state.spec.view |> render(state, _, input)
          actor.continue(state)
        }
      }
    }
    Exit -> {
      { c(ShowCursor) <> c(MainBuffer) } |> io.print
      process.send(state.spec.exit, Nil)
      actor.Stop(process.Normal)
    }
  }
}

fn detect_event(
  state: State(model, msg),
  node: Node(msg),
  input: Key,
) -> Option(msg) {
  case node {
    Input(_, _, _, event) -> None
    HR | HR2(..) | BR -> None
    Text(..) | TextMulti(..) -> None
    Button(_, key, event) if input == key -> Some(event)
    Button(..) -> None
    KeyBind(key, event) if input == key -> Some(event)
    KeyBind(..) -> None
    Div(children, _) | Box(children, _) ->
      do_detect_event(state, children, input)
    Split(splits) ->
      case splits {
        Split2(a:, b:, ..) ->
          detect_event(state, Split(a), input)
          |> option.lazy_or(fn() { detect_event(state, Split(b), input) })
        Split1(node) -> detect_event(state, node, input)
      }
    Debug -> None
  }
}

fn do_detect_event(
  state: State(model, msg),
  children: List(Node(msg)),
  input: Key,
) -> Option(msg) {
  case children {
    [] -> None
    [x, ..xs] ->
      case detect_event(state, x, input) {
        Some(msg) -> Some(msg)
        None -> do_detect_event(state, xs, input)
      }
  }
}

//
// TASKS
//

fn task_handler(tasks: List(fn() -> msg), queue: Subject(Event(msg))) -> Nil {
  list.each(tasks, fn(task) {
    fn() { task() |> Cmd |> process.send(queue, _) }
    |> process.start(False)
  })
}

//
// FIELD FOCUS
//

fn list_focusable(
  children: List(Node(msg)),
  acc: List(Focused(msg)),
) -> List(Focused(msg)) {
  case children {
    [] -> acc
    [x, ..xs] ->
      case x {
        Div(children, _) | Box(children, _) ->
          list_focusable(xs, list_focusable(children, acc))
        Input(width, label, value, event) -> {
          let cursor = string.length(value)
          let focused =
            Focused(label:, value:, event:, offset: 0, cursor:, width:)
          let offset = input_offset(cursor, focused.offset, focused.width)
          list_focusable(xs, [Focused(..focused, offset:), ..acc])
        }
        _ -> list_focusable(xs, acc)
      }
  }
}

fn focus_next(
  focusable: List(Focused(msg)),
  focused: Focused(msg),
  next: Bool,
) -> Option(Focused(msg)) {
  case focusable {
    [] -> None
    [x, ..xs] ->
      case next {
        True -> Some(x)
        False -> focus_next(xs, focused, x == focused)
      }
  }
}

//
// TEXT INPUT
//

fn input_handler(focused: Focused(msg), input: Key) -> Focused(msg) {
  case input {
    key.Backspace -> {
      let cursor = int.max(0, focused.cursor - 1)
      let offset = int.max(0, focused.offset - 1)
      Focused(
        ..focused,
        value: focused.value |> string_backspace(focused.cursor, -1),
        cursor:,
        offset:,
      )
    }
    //key.Up -> string.drop_end(value, 1)
    //key.Down -> string.drop_end(value, 1)
    key.Home -> Focused(..focused, cursor: 0)
    key.End -> Focused(..focused, cursor: string.length(focused.value))
    key.Right -> {
      let cursor = int.min(string.length(focused.value), focused.cursor + 1)
      let offset = input_offset(cursor, focused.offset, focused.width)
      Focused(..focused, cursor:, offset:)
    }
    key.Left -> {
      let cursor = int.max(0, focused.cursor - 1)
      let offset = input_offset(cursor, focused.offset, focused.width)
      Focused(..focused, cursor:, offset:)
    }
    key.Delete -> {
      let offset = case focused.cursor == string.length(focused.value) {
        True -> focused.offset
        False -> int.max(0, focused.offset - 1)
      }
      Focused(
        ..focused,
        value: focused.value |> string_backspace(focused.cursor, 0),
        offset:,
      )
    }
    key.Char(char) -> {
      let cursor = focused.cursor + 1
      let offset = input_offset(cursor, focused.offset, focused.width)
      Focused(
        ..focused,
        value: string_insert(focused.value, focused.cursor, char),
        cursor:,
        offset:,
      )
    }
    _ -> focused
  }
}

fn input_offset(cursor cursor: Int, offset offset: Int, width width: Int) -> Int {
  case cursor {
    x if x < offset -> cursor
    x if x >= offset + { width - 3 } -> cursor - width + 3
    _ -> offset
  }
}

fn string_insert(str: String, cursor: Int, char: String) -> String {
  case cursor {
    x if x <= 0 -> char <> str
    _ -> {
      str
      |> string.to_graphemes
      |> list.index_map(fn(i, idx) {
        case idx == { cursor - 1 } {
          True -> i <> char
          False -> i
        }
      })
      |> string.join("")
    }
  }
}

fn string_backspace(str: String, cursor: Int, offset: Int) -> String {
  str
  |> string.to_graphemes
  |> list.index_fold([], fn(acc, i, idx) {
    case idx == { cursor + offset } {
      True -> acc
      False -> [i, ..acc]
    }
  })
  |> list.reverse
  |> string.join("")
}

//
// CONTROL
//

type Control {
  FocusClear
  FocusNext
  FocusPrev
  ModeInsert
  PassInput(Key)
}

fn control_event(input: Key, keybinds: Keybinds) -> Control {
  case input {
    x if x == keybinds.focus_clear -> FocusClear
    x if x == keybinds.focus_next -> FocusNext
    x if x == keybinds.focus_prev -> FocusPrev
    x if x == keybinds.mode_insert -> ModeInsert
    x -> PassInput(x)
  }
}

//
// RENDER
//

type Pos {
  Pos(x: Int, y: Int, width: Int, height: Int)
}

fn render(state: State(model, msg), node: Node(msg), last_input: Key) {
  let assert Ok(width) = terminal_columns() as "failed to get terminal size"
  let assert Ok(height) = terminal_rows() as "failed to get terminal size"
  let pos = Pos(0, 0, width, height)
  c(BSU) |> io.print
  {
    c(Clear)
    <> node |> render_node(state, _, last_input, pos) |> option.unwrap("")
  }
  |> io.print
  c(ESU) |> io.print
}

fn render_node(
  state: State(model, msg),
  node: Node(msg),
  last_input: Key,
  pos: Pos,
) -> Option(String) {
  case node {
    Split(splits) ->
      case splits {
        Split1(node) -> render_node(state, node, last_input, pos)
        Split2(direction:, ratio:, a:, b:) ->
          case direction {
            Horizontal ->
              [
                c(SetPos(pos.x, pos.y)) |> Some,
                render_node(
                  state,
                  Split(a),
                  last_input,
                  Pos(
                    ..pos,
                    width: pos.width,
                    height: percent(pos.height, ratio.a),
                  ),
                ),
                c(SetPos(pos.x + 1 + percent(pos.height, ratio.a), pos.y))
                  |> Some,
                render_node(
                  state,
                  Split(b),
                  last_input,
                  Pos(
                    x: percent(pos.height, ratio.a) + 1,
                    y: pos.y,
                    width: pos.width,
                    height: percent(pos.height, ratio.b),
                  ),
                ),
              ]
              |> option.values
              |> string.join("")
              |> Some
            Vertical ->
              [
                c(SetPos(pos.x, pos.y)) |> Some,
                render_node(
                  state,
                  Split(a),
                  last_input,
                  Pos(
                    ..pos,
                    width: percent(pos.width, ratio.a),
                    height: pos.height,
                  ),
                ),
                c(SetPos(pos.x, pos.y + 1 + percent(pos.width, ratio.a)))
                  |> Some,
                render_node(
                  state,
                  Split(b),
                  last_input,
                  Pos(
                    x: pos.x,
                    y: percent(pos.width, ratio.a) + 1,
                    width: percent(pos.width, ratio.b),
                    height: pos.height,
                  ),
                ),
              ]
              |> option.values
              |> string.join("")
              |> Some
          }
      }
    Debug -> string.inspect(pos) |> Some
    Div(children, separator) ->
      list.map(children, render_node(state, _, last_input, pos))
      |> option.values
      |> string.join(sep(separator))
      |> Some
    Box(children, title) -> {
      let pos = Pos(..pos, width: pos.width - 4, height: pos.height - 2)
      [
        draw_box(pos.width, pos.height + 1, title),
        ..list.map(children, render_node(state, _, last_input, pos))
        |> option.values
      ]
      |> string.join(sep(In))
      |> Some
    }
    Button(text, input, _) ->
      draw_btn(Btn(10, 1, "", text, last_input == input)) |> Some
    KeyBind(..) -> None
    Input(width, label, value, _event) -> {
      let #(is_focused, cursor) = case state.focused {
        Some(focused) if focused.label == label -> #(True, focused.cursor)
        _ -> #(False, string.length(value))
      }
      let is_insert = case state.mode {
        Insert -> True
        _ -> False
      }
      let offset = case state.focused {
        Some(focused) if focused.label == label -> focused.offset
        _ -> input_offset(string.length(value), 0, width)
      }
      draw_input(Iput(
        width,
        1,
        label,
        value,
        is_focused,
        is_insert,
        cursor,
        offset,
      ))
      |> Some
    }
    Text(text, fg, bg) ->
      {
        { option.map(fg, fn(o) { c(Fg(o)) }) |> option.unwrap("") }
        <> { option.map(bg, fn(o) { c(Bg(o)) }) |> option.unwrap("") }
        <> text |> string.slice(0, pos.width - 2)
        <> c(Reset)
      }
      |> Some

    TextMulti(text, fg, bg) ->
      {
        { option.map(fg, fn(o) { c(Fg(o)) }) |> option.unwrap("") }
        <> { option.map(bg, fn(o) { c(Bg(o)) }) |> option.unwrap("") }
        <> text |> text_to_multi(pos.width, pos.height)
        <> c(Reset)
      }
      |> Some

    //HR -> terminal_columns() |> result.unwrap(0) |> string.repeat("─", _)
    HR -> string.repeat("─", pos.width) |> Some
    HR2(color) ->
      { c(Fg(color)) <> string.repeat("─", pos.width) <> c(Reset) } |> Some
    BR -> "\n" |> Some
  }
}

fn text_to_multi(text: String, width: Int, height: Int) -> String {
  c(SavePos)
  <> {
    text
    |> string.split("\n")
    |> list.take(height)
    |> list.map(string.slice(_, 0, width - 2))
    |> string.join(c(LoadPos) <> c(Down(1)) <> c(SavePos))
  }
  <> c(LoadPos)
  <> c(Down(1))
}

fn sep(separator: Separator) -> String {
  case separator {
    Row -> c(Right(1))
    Col -> c(Down(1)) <> c(StartLine)
    In -> c(LoadPos) <> c(Down(1)) <> c(SavePos)
  }
}

fn percent(x: Int, r: Int) -> Int {
  x * r / 100
}

//
// READ INPUT
//

@external(erlang, "io", "get_line")
fn get_line(prompt: String) -> Charlist

@external(erlang, "io", "get_chars")
fn get_chars(prompt: String, count: Int) -> String

fn read_input(shore: Subject(Event(msg)), exit: Key) -> Nil {
  // TODO: this seems to solve issues with key seuqences but:
  // a) is 10 long enough for expected key codes
  // b) is it possible to have character merges if you press two keys quickly enough?
  let key = get_chars("", 10) |> key.from_string
  case key == exit {
    True -> Exit |> process.send(shore, _)
    False -> key |> KeyPress |> process.send(shore, _)
  }
  read_input(shore, exit)
}

//
// DRAW
//

pub type Node(msg) {
  /// A field for text input
  Input(width: Int, label: String, value: String, event: fn(String) -> msg)
  /// A horizontal line
  HR
  HR2(color: Color)
  /// An empty line
  BR
  /// A text string
  Text(text: String, fg: Option(Color), bg: Option(Color))
  /// A multi-line text string
  TextMulti(text: String, fg: Option(Color), bg: Option(Color))
  /// A button assigned to a key press to execute an event
  Button(text: String, key: Key, event: msg)
  /// A non-visible button assigned to a key press to execute an event
  KeyBind(key: Key, event: msg)
  /// A container element for holding other nodes
  Div(children: List(Node(msg)), separator: Separator)
  /// A box container element for holding other nodes
  Box(children: List(Node(msg)), title: Option(String))
  ///
  Split(Splits(msg))
  Debug
}

/// TODO
pub type Splits(msg) {
  Split1(node: Node(msg))
  Split2(direction: Direction, ratio: Ratio2, a: Splits(msg), b: Splits(msg))
}

pub type Ratio2 {
  Ratio2(a: Int, b: Int)
}

pub type Direction {
  Horizontal
  Vertical
}

pub type Separator {
  Row
  Col
  In
}

fn do_middle(width: Int, height: Int, acc: List(String)) -> String {
  case height {
    0 -> acc |> string.join(c(Left(width + 2)) <> c(Down(1)))
    h -> do_middle(width, h - 1, [middle(width), ..acc])
  }
}

fn middle(width: Int) -> String {
  let fill = " "
  ["│", string.repeat(fill, width), "│"] |> string.join("")
}

type Btn {
  Btn(width: Int, height: Int, title: String, text: String, pressed: Bool)
}

type Iput {
  Iput(
    width: Int,
    height: Int,
    title: String,
    text: String,
    pressed: Bool,
    insert: Bool,
    cursor: Int,
    offset: Int,
  )
}

fn draw_input(btn: Iput) -> String {
  // 2 == padding of space either side
  let color = case btn.pressed, btn.insert {
    True, True -> Green |> Fg |> c
    True, False -> Blue |> Fg |> c
    _, _ -> White |> Fg |> c
  }
  let in_width = btn.width - 2
  let text = btn.text <> " "
  let padding = in_width - string.length(text)
  let text_trim =
    text
    |> string.slice(btn.offset, in_width)
    //|> map_cursor(btn.cursor - btn.offset, btn.width)
    |> fn(x) {
      case btn.pressed {
        True -> map_cursor(x, btn.cursor - btn.offset, btn.width)
        False -> x
      }
    }
  let top = case string.length(btn.title) {
    0 -> fn() { ["╭", string.repeat("─", btn.width), "╮"] |> string.join("") }
    _ -> fn() {
      [
        "╭",
        "─",
        " ",
        btn.title,
        " ",
        string.repeat("─", btn.width - 3 - string.length(btn.title)),
        "╮",
      ]
      |> string.join("")
    }
  }
  let middle = fn() {
    [
      "│",
      " ",
      c(Reset),
      text_trim,
      string.repeat(" ", padding),
      color,
      " ",
      "│",
    ]
    |> string.join("")
  }
  let bottom = fn() {
    ["╰", string.repeat("─", btn.width), "╯"] |> string.join("")
  }
  let width = btn.width + 2
  let start = c(Left(width)) <> c(Down(1))
  let top_right = c(Up(1 + btn.height))
  [
    color,
    top(),
    start,
    middle(),
    start,
    bottom(),
    top_right,
    // // DEBUG
    //" ",
    //int.to_string(btn.cursor),
    //" ",
    //btn.offset |> int.to_string,
    //" ",
    //btn.text |> string.length |> int.to_string,
    //" ",
    //padding |> int.to_string,
    //" ",
    //text_trim |> string.length |> int.to_string,
    Reset |> c,
  ]
  |> string.join("")
}

fn map_cursor(str: String, cursor: Int, width: Int) -> String {
  let pos = cursor |> int.min(width - 2)
  str
  |> string.to_graphemes
  |> list.index_map(fn(i, idx) {
    case idx == pos {
      True -> c(Bg(White)) <> c(Fg(Black)) <> i <> c(Reset)
      False -> i
    }
  })
  |> string.join("")
}

fn draw_btn(btn: Btn) -> String {
  let padding = { btn.width - string.length(btn.text) |> int.to_float } /. 2.0
  let odd = case float.modulo(padding, 2.0) {
    Ok(0.0) -> 0
    Ok(1.0) -> 0
    Ok(_) -> 1
    _ -> 0
  }
  let padl = float.truncate(padding)
  let padr = float.truncate(padding) + odd
  let top = case string.length(btn.title) {
    0 -> fn() { ["╭", string.repeat("─", btn.width), "╮"] |> string.join("") }
    _ -> fn() {
      [
        "╭",
        "─",
        " ",
        btn.title,
        " ",
        string.repeat("─", btn.width - 3 - string.length(btn.title)),
        "╮",
      ]
      |> string.join("")
    }
  }
  let middle = fn() {
    ["│", string.repeat(" ", padl), btn.text, string.repeat(" ", padr), "│"]
    |> string.join("")
  }
  let bottom = fn() {
    ["╰", string.repeat("─", btn.width), "╯"] |> string.join("")
  }
  let width = btn.width + 2
  let start = c(Left(width)) <> c(Down(1))
  let top_right = c(Up(1 + btn.height))
  let color = case btn.pressed {
    True -> Blue |> Fg |> c
    False -> White |> Fg |> c
  }
  [color, top(), start, middle(), start, bottom(), top_right, Reset |> c]
  |> string.join("")
}

fn draw_box(width: Int, height: Int, title: Option(String)) -> String {
  let top =
    case title {
      Some(title) -> [
        "╭",
        "─",
        " ",
        title,
        " ",
        string.repeat("─", width - 3 - string.length(title)),
        "╮",
      ]
      None -> ["╭", string.repeat("─", width), "╮"]
    }
    |> string.join("")
  let bottom = ["╰", string.repeat("─", width), "╯"] |> string.join("")
  let start = c(Left(width + 2)) <> c(Down(1))
  [
    c(SavePos),
    top,
    start,
    do_middle(width, height, []),
    start,
    bottom,
    c(LoadPos),
    c(Right(2)),
    c(SavePos),
  ]
  |> string.join("")
}

//
// TERMINAL CODES
//

const esc = "\u{001b}"

type TermCode {
  Clear
  Top
  HideCursor
  ShowCursor
  SetPos(x: Int, y: Int)
  SavePos
  LoadPos
  Up(Int)
  Down(Int)
  Left(Int)
  Right(Int)
  StartLine
  Column(Int)
  Fg(Color)
  Bg(Color)
  Reset
  GetPos
  AltBuffer
  MainBuffer
  /// Begin Synchronized Output
  BSU
  /// End Synchronized Output
  ESU
}

fn c(code: TermCode) -> String {
  case code {
    Clear -> esc <> "[2J" <> esc <> "[H"
    Top -> esc <> "[H"
    HideCursor -> esc <> "[?25l"
    ShowCursor -> esc <> "[?25h"
    SetPos(x, y) ->
      esc <> "[" <> int.to_string(x) <> ";" <> int.to_string(y) <> "H"
    SavePos -> esc <> "[s"
    LoadPos -> esc <> "[u"
    Up(i) -> esc <> "[" <> int.to_string(i) <> "A"
    Down(i) -> esc <> "[" <> int.to_string(i) <> "B"
    Left(i) -> esc <> "[" <> int.to_string(i) <> "D"
    Right(i) -> esc <> "[" <> int.to_string(i) <> "C"
    StartLine -> Column(1) |> c
    Column(i) -> esc <> "[" <> int.to_string(i) <> "G"
    Fg(color) -> esc <> "[3" <> col(color) <> "m"
    Bg(color) -> esc <> "[4" <> col(color) <> "m"
    Reset -> esc <> "[0m"
    GetPos -> esc <> "[6n"
    AltBuffer -> esc <> "[?1049h"
    MainBuffer -> esc <> "[?1049l"
    BSU -> esc <> "[?2026h"
    ESU -> esc <> "[?2026l"
  }
}

//
// COLOR
//

pub type Color {
  Black
  Red
  Green
  Yellow
  Blue
  Magenta
  Cyan
  White
}

fn col(color: Color) -> String {
  case color {
    Black -> "0"
    Red -> "1"
    Green -> "2"
    Yellow -> "3"
    Blue -> "4"
    Magenta -> "5"
    Cyan -> "6"
    White -> "7"
  }
}

//
// SHELL
//

type ShellOpt {
  Noshell
  Raw
}

fn raw_erl() {
  raw_ffi(#(Noshell, Raw))
}

type DoNotLeak

@external(erlang, "shell", "start_interactive")
fn raw_ffi(opts: #(ShellOpt, ShellOpt)) -> DoNotLeak

type TODO

@external(erlang, "io", "rows")
fn terminal_rows() -> Result(Int, TODO)

@external(erlang, "io", "columns")
fn terminal_columns() -> Result(Int, TODO)

// TODO: probably just delete this?
fn get_pos() -> #(Int, Int) {
  c(GetPos) |> io.print

  get_line("")
  |> charlist.to_string()
  |> string.drop_start(2)
  |> string.drop_end(1)
  |> string.split(";")
  |> list.map(int.parse)
  |> fn(i) {
    case i {
      [Ok(a), Ok(b)] -> #(a, b) |> echo
      fixme -> {
        fixme |> echo
        #(-1, -1)
      }
    }
  }
}
