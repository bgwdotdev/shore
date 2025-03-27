import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/pair
import gleam/result
import gleam/string

pub fn main_old() {
  io.println("Hello from shore!")
  Clear |> terminal
  set_cbreak() |> echo
  draw(Input) |> io.println
  Pos(15, 10) |> terminal
  draw(Input) |> io.println
  //process.start(read_input, False)
  process.sleep_forever()
}

pub fn main() {
  elm_main()
  Nil
}

fn start(spec: Spec(model)) {
  set_cbreak()
  let assert Ok(elm) = elm_start(spec)
  process.start(fn() { read_input(elm) }, False)
  process.sleep_forever()
}

fn elm_start(spec: Spec(model)) {
  actor.start(spec, event_loop_actor)
}

type Spec(model) {
  Spec(
    model: model,
    init: fn() -> model,
    view: fn(model) -> Node,
    update: fn(model, Msg) -> model,
  )
}

//
// ELM
//

fn elm_main() {
  Spec(model: init(), init:, view:, update:) |> start
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
  Div([
    model.counter |> int.to_string |> Text,
    Button("increment", "a", Increment),
    Button("decrement", "b", Decrement),
  ])
}

//
// EVENT BUS(?)
//

fn detect_event(node: Node, input: String) -> Option(Msg) {
  case node {
    Input -> None
    Text(_) -> None
    Button(_, key, event) if input == key -> Some(event)
    Button(_, _, _) -> None
    Div(children) -> do_detect_event(children, input)
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

// TODO: pass model/view/update as a type
fn event_loop(model: Model, input: String) {
  let ui = view(model)
  case detect_event(ui, input) {
    Some(msg) -> {
      // TODO: send to subject and return model??
      let model = update(model, msg)
      model |> view |> render
      model
    }
    None -> model
  }
  // TODO: actor.continue
}

fn event_loop_actor(input: String, spec: Spec(model)) {
  let ui = spec.view(spec.model)
  let model = case detect_event(ui, input) {
    Some(msg) -> {
      // TODO: send to subject and return model??
      let model = spec.update(spec.model, msg)
      model |> spec.view |> render
      model
    }
    None -> spec.model
  }
  actor.continue(Spec(..spec, model: model))
}

fn render(node: Node) {
  Clear |> terminal
  echo node
}

//
// RUNTIME
//

@external(erlang, "shore_ffi", "setCbreak_nif")
fn set_cbreak() -> Nil

type Data

@external(erlang, "io", "get_chars")
fn get_chars(prompt: String, count: Int) -> String

fn read_input(elm) {
  get_chars("", 1) |> process.send(elm, _)
  read_input(elm)
}

type Node {
  Input
  Text(text: String)
  Button(text: String, key: String, event: Msg)
  Div(List(Node))
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

@external(erlang, "shore_ffi", "terminal")
fn terminal(code: TermCode) -> Charlist

type TermCode {
  Clear
  Top
  Pos(x: Int, y: Int)
  Up(Int)
  Down(Int)
  Left(Int)
  Right(Int)
}

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
