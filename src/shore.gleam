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

type Focused(msg) {
  Focused(label: String, value: String, event: fn(String) -> msg, cursor: Int)
}

type Mode {
  Insert
  Normal
}

pub fn start(spec: Spec(model, msg)) -> Subject(Event(msg)) {
  raw_erl()
  let assert Ok(shore) = shore_start(spec)
  process.start(fn() { read_input(shore) }, False)
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
      input |> echo
      case state.mode {
        Normal -> {
          let ui = state.spec.view(state.model)
          let state = case control_event(input) {
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
          let mode = case input {
            key.Esc -> Normal
            _ -> Insert
          }
          let #(focused, model) = case state.focused {
            Some(focused) -> {
              let new_value = input_handler(focused.value, input)
              let #(model, tasks) =
                state.spec.update(state.model, focused.event(new_value))
              tasks |> task_handler(state.tasks)
              #(Some(Focused(..focused, value: new_value)), model)
            }
            x -> #(x, state.model)
          }
          let state = State(..state, focused:, mode:, model:)
          state.model |> state.spec.view |> render(state, _, input)
          actor.continue(state)
        }
      }
    }
    Exit -> actor.Stop(process.Normal)
  }
}

fn input_handler(value: String, input: Key) -> String {
  case input {
    key.Backspace -> value |> string.drop_end(1)
    key.Up -> string.drop_end(value, 1)
    key.Down -> string.drop_end(value, 1)
    key.Right -> string.drop_end(value, 1)
    key.Left -> string.drop_end(value, 1)
    key.Delete -> value
    key.Char(char) -> value <> char
    _ -> value
  }
}

fn task_handler(tasks: List(fn() -> msg), queue: Subject(Event(msg))) -> Nil {
  list.each(tasks, fn(task) {
    fn() { task() |> Cmd |> process.send(queue, _) }
    |> process.start(False)
  })
}

//
// CONTROL
//

type Control {
  FocusClear
  FocusNext
  FocusPrev
  ModeInsert
  //Quit
  PassInput(Key)
}

fn control_event(input: Key) -> Control {
  case input {
    key.Char("J") -> FocusNext
    key.Char("K") -> FocusPrev
    key.Char("I") -> ModeInsert
    //"f" -> todo
    //"Q" -> quit
    key.Esc -> FocusClear
    x -> PassInput(x)
  }
}

fn list_focusable(
  children: List(Node(msg)),
  acc: List(Focused(msg)),
) -> List(Focused(msg)) {
  case children {
    [] -> acc
    [x, ..xs] ->
      case x {
        Div(children, _) -> list_focusable(xs, list_focusable(children, acc))
        Input(label, value, event) ->
          list_focusable(xs, [
            Focused(label:, value:, event:, cursor: string.length(value)),
            ..acc
          ])
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

fn detect_event(
  state: State(model, msg),
  node: Node(msg),
  input: Key,
) -> Option(msg) {
  case node {
    Input(_, _, event) -> None
    HR | BR -> None
    Text(_, _) -> None
    Button(_, key, event) if input == key.Char(key) -> Some(event)
    Button(_, _, _) -> None
    Div(children, _) -> do_detect_event(state, children, input)
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
// RENDER
//

fn render(state: State(model, msg), node: Node(msg), last_input: Key) {
  { c(Clear) <> node |> render_node(state, _, last_input) }
  |> io.print
}

fn render_node(
  state: State(model, msg),
  node: Node(msg),
  last_input: Key,
) -> String {
  case node {
    Div(children, separator) ->
      list.map(children, render_node(state, _, last_input))
      |> string.join(sep(separator))
    Button(text, input, _) ->
      draw_btn(Btn(10, 1, "", text, last_input == key.Char(input)))
    Input(label, value, _event) -> {
      let is_focused = case state.focused {
        Some(focused) -> focused.label == label
        _ -> False
      }
      draw_input(Btn(20, 1, label, value, is_focused))
    }
    Text(text, fg) ->
      { option.map(fg, fn(o) { c(Fg(o)) }) |> option.unwrap("") }
      <> text
      <> c(Reset)
    HR -> terminal_columns() |> result.unwrap(0) |> string.repeat("─", _)
    BR -> "\n"
    _ -> ""
  }
}

fn sep(separator: Separator) -> String {
  case separator {
    Row -> c(Right(1))
    Col -> c(Down(1)) <> c(StartLine)
  }
}

//
// READ INPUT
//

@external(erlang, "io", "get_line")
fn get_line(prompt: String) -> Charlist

@external(erlang, "io", "get_chars")
fn get_chars(prompt: String, count: Int) -> String

fn read_input(shore: Subject(Event(msg))) -> Nil {
  // TODO: this seems to solve issues with key seuqences but:
  // a) is 10 long enough for expected key codes
  // b) is it possible to have character merges if you press two keys quickly enough?
  get_chars("", 10) |> key.from_string |> KeyPress |> process.send(shore, _)
  read_input(shore)
}

//
// DRAW
//

pub type Node(msg) {
  Input(label: String, value: String, event: fn(String) -> msg)
  HR
  BR
  Text(text: String, fg: Option(Color))
  Button(text: String, key: String, event: msg)
  Div(children: List(Node(msg)), separator: Separator)
}

pub type Separator {
  Row
  Col
}

fn do_middle(height: Int, acc: List(String)) -> String {
  case height {
    0 -> acc |> string.join("\n")
    h -> do_middle(h - 1, [middle(), ..acc])
  }
}

fn middle() -> String {
  let fill = " "
  let width = 30
  ["│", string.repeat(fill, width), "│"] |> string.join("")
}

type Btn {
  Btn(width: Int, height: Int, title: String, text: String, pressed: Bool)
}

fn draw_input(btn: Btn) -> String {
  let padding = btn.width - string.length(btn.text) |> int.to_float
  let odd = case float.modulo(padding, 2.0) {
    Ok(0.0) -> 0
    Ok(1.0) -> 0
    Ok(_) -> 1
    _ -> 0
  }
  let padr = float.truncate(padding) + 0 - 2
  let text_trim =
    string.length(btn.text) + 2 - btn.width
    |> int.max(0)
    |> string.drop_start(btn.text, _)
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
    ["│", " ", text_trim, string.repeat(" ", padr), " ", "│"]
    |> string.join("")
  }
  let bottom = fn() {
    ["╰", string.repeat("─", btn.width), "╯"] |> string.join("")
  }
  let width = btn.width + 2
  let start = c(Left(width)) <> c(Down(1))
  let top_right = c(Up(1 + btn.height))
  let colour = case btn.pressed {
    True -> Blue |> Fg |> c
    False -> White |> Fg |> c
  }
  [
    colour,
    top(),
    start,
    middle(),
    start,
    bottom(),
    top_right,
    " ",
    float.to_string(padding),
    " ",
    int.to_string(odd),
    " ",
    float.modulo(padding, 2.0) |> string.inspect,
    Reset |> c,
  ]
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
  let colour = case btn.pressed {
    True -> Blue |> Fg |> c
    False -> White |> Fg |> c
  }
  [colour, top(), start, middle(), start, bottom(), top_right, Reset |> c]
  |> string.join("")
}

fn draw(node: Node(msg)) -> String {
  let width = 30
  let height = 1
  let fill = "█"
  let fill = " "
  let title = "username"
  let top = fn() { ["╭", string.repeat("─", width), "╮"] |> string.join("") }
  let top = fn() {
    [
      "╭",
      "─",
      " ",
      "username",
      " ",
      string.repeat("─", width - 3 - string.length(title)),
      "╮",
    ]
    |> string.join("")
  }
  let bottom = fn() { ["╰", string.repeat("─", width), "╯"] |> string.join("") }
  [top(), do_middle(height, []), bottom()] |> string.join("\n")
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
  Pos(x: Int, y: Int)
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
}

fn c(code: TermCode) -> String {
  case code {
    Clear -> esc <> "[2J" <> esc <> "[H"
    Top -> esc <> "[H"
    HideCursor -> esc <> "[?25l"
    ShowCursor -> esc <> "[?25h"
    Pos(x, y) ->
      esc <> "[" <> int.to_string(x) <> ";" <> int.to_string(y) <> "H"
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
  }
}

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
      [Ok(a), Ok(b)] -> #(a, b)
      fixme -> {
        fixme |> echo
        #(-1, -1)
      }
    }
  }
}

type TODO

@external(erlang, "io", "rows")
fn terminal_rows() -> Result(Int, TODO)

@external(erlang, "io", "columns")
fn terminal_columns() -> Result(Int, TODO)
