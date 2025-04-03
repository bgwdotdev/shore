import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/pair
import gleam/result
import gleam/string

pub fn main_old() {
  io.println("Hello from shore!")

  hello_nif()
  size() |> echo
  //Clear |> c |> io.print
  //set_cbreak() |> echo
  //draw(Input) |> io.println
  //Pos(15, 10) |> c |> io.print
  //draw(Input) |> io.println
  process.sleep_forever()
}

pub fn main() {
  elm_main()
  //main_old()
  Nil
}

fn start(spec: Spec(model)) {
  set_cbreak()
  let assert Ok(elm) = elm_start(spec)
  process.start(fn() { read_input(elm) }, False)
  process.sleep_forever()
}

fn elm_start(spec: Spec(model)) {
  let _on_init = spec.model |> spec.view |> render("")
  let state = State(spec:, last_input: "")
  actor.start(state, event_loop_actor)
}

type State(model) {
  State(spec: Spec(model), last_input: String)
}

type Spec(model) {
  Spec(model: model, view: fn(model) -> Node, update: fn(model, Msg) -> model)
}

//
// ELM
//

fn elm_main() {
  Spec(model: init(), view:, update:) |> start
}

type Model {
  Model(counter: Int)
}

fn init() -> Model {
  Model(counter: 0)
}

type Msg {
  Increment
  Decrement
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    Increment -> Model(model.counter + 1)
    Decrement -> Model(model.counter - 1)
  }
}

fn view(model: Model) -> Node {
  Div(
    [
      "HELLO WORLD" |> Text(None),
      HR,
      model.counter |> int.to_string |> Text(Some(Black)),
      model.counter |> int.to_string |> Text(Some(Red)),
      model.counter |> int.to_string |> Text(Some(Green)),
      model.counter |> int.to_string |> Text(Some(Yellow)),
      model.counter |> int.to_string |> Text(Some(Blue)),
      model.counter |> int.to_string |> Text(Some(Magenta)),
      model.counter |> int.to_string |> Text(Some(Cyan)),
      model.counter |> int.to_string |> Text(Some(White)),
      BR,
      Div([Button("++", "a", Increment), Button("--", "b", Decrement)], Row),
      case model.counter {
        x if x > 10 && x < 20 -> Button("cc", "c", Increment)
        x if x > 20 -> Button("dd", "d", Increment)
        x -> Text("x", Some(Red))
      },
    ],
    Col,
  )
}

//
// EVENT BUS(?)
//

fn detect_event(node: Node, input: String) -> Option(Msg) {
  case node {
    Input(_) -> None
    HR | BR -> None
    Text(_, _) -> None
    Button(_, key, event) if input == key -> Some(event)
    Button(_, _, _) -> None
    Div(children, _) -> do_detect_event(children, input)
  }
}

fn do_detect_event(children: List(Node), input: String) -> Option(Msg) {
  case children {
    [] -> None
    [x, ..xs] ->
      case detect_event(x, input) {
        Some(msg) -> Some(msg)
        None -> do_detect_event(xs, input)
      }
  }
}

fn event_loop_actor(input: String, state: State(model)) {
  let ui = state.spec.view(state.spec.model)
  let model = case detect_event(ui, input) {
    Some(msg) -> {
      // TODO: send to subject and return model??
      let model = state.spec.update(state.spec.model, msg)
      model |> state.spec.view |> render(input)
      model
    }
    None -> state.spec.model
  }
  let state = State(..state, spec: Spec(..state.spec, model: model))
  actor.continue(state)
}

fn render(node: Node, last_input: String) {
  let _size = size()
  { c(Clear) <> node |> render_node(last_input) }
  |> io.print
}

fn render_node(node: Node, last_input: String) -> String {
  case node {
    Div(children, separator) ->
      list.map(children, render_node(_, last_input))
      |> string.join(sep(separator))
    Button(text, input, _) -> draw_btn(Btn(10, 1, text, last_input == input))
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
    Row -> " "
    Col -> "\n"
  }
}

//
// RUNTIME
//

@external(erlang, "shore_ffi", "size_nif")
fn size() -> #(Int, Int)

@external(erlang, "shore_ffi", "cbreak_nif")
fn set_cbreak() -> Nil

type Data

@external(erlang, "io", "get_chars")
fn get_chars(prompt: String, count: Int) -> String

fn read_input(elm) {
  get_chars("", 1) |> process.send(elm, _)
  read_input(elm)
}

type Node {
  Input(label: String)
  HR
  BR
  Text(text: String, fg: Option(Color))
  Button(text: String, key: String, event: Msg)
  Div(children: List(Node), separator: Separator)
}

type Separator {
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
  Btn(width: Int, height: Int, text: String, pressed: Bool)
}

fn draw_btn(btn: Btn) -> String {
  let padding = { btn.width - string.length(btn.text) } / 2
  let top = fn() {
    ["╭", string.repeat("─", btn.width), "╮"] |> string.join("")
  }
  let middle = fn() {
    [
      "│",
      string.repeat(" ", padding),
      btn.text,
      string.repeat(" ", padding),
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
  let colour = case btn.pressed {
    True -> Blue |> Fg |> c
    False -> White |> Fg |> c
  }
  [colour, top(), start, middle(), start, bottom(), top_right, Reset |> c]
  |> string.join("")
}

fn draw(node: Node) -> String {
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
  //let box =
  //  [
  //    "╭",
  //    string.repeat("─", width),
  //    "╮",
  //    "\n",
  //    "│",
  //    string.repeat(fill, width),
  //    "│",
  //    "\n",
  //    "╰",
  //    string.repeat("─", width),
  //    "╯",
  //  ]
  //  |> string.join("")
}

@external(erlang, "shore_ffi", "hello_nif")
fn hello_nif() -> Int

@external(erlang, "shore_ffi", "cmd")
fn do_cmd(input: Charlist) -> Charlist

@external(erlang, "shore_ffi", "get_pos")
fn do_get_pos() -> Charlist

fn get_pos() -> #(Int, Int) {
  do_get_pos()
  |> echo
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
  Fg(Color)
  Bg(Color)
  Reset
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
    Fg(color) -> esc <> "[3" <> col(color) <> "m"
    Bg(color) -> esc <> "[4" <> col(color) <> "m"
    Reset -> esc <> "[0m"
  }
}

type Color {
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
// DELETE THIS PROBABLY?
//

fn cmd(input: String) -> String {
  input
  |> charlist.from_string
  |> do_cmd
  |> charlist.to_string
}

fn raw() -> String {
  cmd("stty raw -echo")
}

fn cbreak() -> String {
  cmd("stty -icanon min 1 -echo")
}

fn reset() -> String {
  cmd("stty sane")
}
