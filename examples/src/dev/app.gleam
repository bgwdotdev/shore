import gleam/bit_array
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/pair
import gleam/string
import shore
import shore/key
import shore/style
import shore/ui

// MAIN

pub fn main() {
  let exit = process.new_subject()
  let ui =
    shore.spec(
      init:,
      view:,
      update:,
      exit:,
      keybinds: shore.default_keybinds(),
      redraw: shore.on_update(),
    )
    |> shore.start
  //tick(ui)
  process.receive_forever(exit)
}

fn tick(ui: process.Subject(shore.Event(Msg))) {
  process.send(ui, shore.send(Tick))
  process.sleep(1000)
  tick(ui)
}

// MODEL 

type Model {
  Model(counter: Int, hi: String, bye: String, csv: List(List(String)))
}

fn init() -> #(Model, List(fn() -> Msg)) {
  let csv = [
    ["name", "age", "date"],
    ["bob", "20", "2025-01-01"],
    ["alice the quick brown fox", "30", "2025-03-04"],
    ["jane", "54", "2024-10-10"],
    ["phil", "33", "2025-02-11"],
  ]
  let model =
    Model(counter: 0, hi: "abcdefghijklmnopqrstuvwxyz", bye: "abcdarsd", csv:)
  #(model, [])
}

// UPDATE

type Msg {
  NoOp
  FixMe(String)
  Increment
  Decrement
  SendReset
  Reset
  Set(Int)
  Tick
  SetHi(String)
  SetBye(String)
}

fn update(model: Model, msg: Msg) -> #(Model, List(fn() -> Msg)) {
  case msg {
    NoOp -> #(model, [])
    FixMe(_) -> #(model, [])
    Increment -> Model(..model, counter: model.counter + 1) |> pair.new([])
    Decrement -> Model(..model, counter: model.counter - 1) |> pair.new([])
    SendReset -> #(model, [reset])
    Reset -> #(Model(..model, counter: 0), [])
    Set(i) -> #(Model(..model, counter: i), [])
    Tick -> #(Model(..model, counter: model.counter + 1), [])
    SetHi(text) -> #(Model(..model, hi: text), [])
    SetBye(text) -> #(Model(..model, bye: text), [])
  }
}

// VIEW

fn view(model: Model) -> shore.Node(Msg) {
  ui.col([
    "HELLO WORLD" |> ui.text,
    ui.hr(),
    ui.input("hi:", model.hi, style.Px(20), SetHi),
    ui.br(),
    model.hi |> ui.text,
    ui.br(),
    ui.input("bye:", model.bye, style.Px(25), SetBye),
    ui.br(),
    model.bye |> ui.text,
    ui.br(),
    ui.table(style.Px(50), model.csv),
    ui.br(),
    ui.image_unstable(pic),
    ui.br(),
    ui.br(),
    ui.text("hi"),
    ui.text_wrapped(
      "The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. 

The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog.",
    ),
    //ui.Progress(ui.Px(20), 100, model.counter, ui.Blue),
    ui.br(),
    model.counter |> int.to_string |> ui.text_styled(Some(style.Black), None),
    model.counter |> int.to_string |> ui.text_styled(Some(style.Red), None),
    model.counter |> int.to_string |> ui.text_styled(Some(style.Green), None),
    model.counter |> int.to_string |> ui.text_styled(Some(style.Yellow), None),
    model.counter |> int.to_string |> ui.text_styled(Some(style.Blue), None),
    model.counter |> int.to_string |> ui.text_styled(Some(style.Magenta), None),
    model.counter |> int.to_string |> ui.text_styled(Some(style.Cyan), None),
    model.counter |> int.to_string |> ui.text_styled(Some(style.White), None),
    ui.br(),
    ui.row([
      ui.button("a: Increment", key.Char("a"), Increment),
      ui.button_styled(
        "b: Decrement",
        key.Char("b"),
        Decrement,
        None,
        None,
        Some(style.Black),
        Some(style.Green),
      ),
    ]),
    case model.counter {
      x if x > 10 && x < 20 -> ui.button("reset", key.Char("r"), SendReset)
      x if x > 20 -> ui.button("dd", key.Char("d"), Set(0))
      x -> ui.text_styled("x", Some(style.Red), None)
    },
    ui.br(),
    ui.graph(style.Px(60), style.Px(7), [
      1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0,
      6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0,
      1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0,
      6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0,
      1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0,
      6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0,
      1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0,
      6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0,
      1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0,
      6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0,
      1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0,
    ]),
    ui.text("hi"),
  ])
}

// CMDS

fn reset() -> Msg {
  process.sleep(10_000)
  Reset
}

const pic = "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII"
