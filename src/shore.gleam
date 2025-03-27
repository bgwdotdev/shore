import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/pair
import gleam/result
import gleam/string

pub fn main() {
  io.println("Hello from shore!")
  Clear |> terminal
  set_cbreak() |> echo
  draw(Input) |> io.println
  Pos(15, 10) |> terminal
  draw(Input) |> io.println
  process.start(read_input, False)
  process.sleep_forever()
}

@external(erlang, "shore_ffi", "setCbreak_nif")
fn set_cbreak() -> Nil

type Data

@external(erlang, "io", "get_chars")
fn get_chars(prompt: String, count: Int) -> String

fn read_input() {
  get_chars("", 1)
  read_input()
}

type Node {
  Input
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
