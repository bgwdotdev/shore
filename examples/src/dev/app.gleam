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
  let _shore =
    shore.spec(
      init:,
      view:,
      update:,
      exit:,
      keybinds: shore.default_keybinds(),
      redraw: shore.on_update(),
    )
    |> shore.start
  process.receive_forever(exit)
}

// MODEL 

type Model {
  Model(
    counter: Int,
    name: String,
    secret: String,
    csv: List(List(String)),
    kv: List(List(String)),
  )
}

fn init() -> #(Model, List(fn() -> Msg)) {
  let csv = [
    ["name", "age", "date"],
    ["bob", "20", "2025-01-01"],
    ["alice the quick brown fox", "30", "2025-03-04"],
    ["jane", "54", "2024-10-10"],
    ["phil", "33", "2025-02-11"],
  ]
  let kv = [
    ["server", "dev01"],
    ["status", "up"],
    ["ip", "192.168.1.1"],
    ["uptime", "1 week"],
  ]
  let model = Model(counter: 0, name: "world", secret: "hunter2", csv:, kv:)
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
  SetName(String)
  SetSecret(String)
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
    SetName(name) -> #(Model(..model, name:), [])
    SetSecret(secret) -> #(Model(..model, secret:), [])
  }
}

// VIEW

fn view(model: Model) -> shore.Node(Msg) {
  ui.col([
    ui.bar2(
      style.Blue,
      ui.text_styled(
        "Development Reference",
        Some(style.Black),
        Some(style.Blue),
      ),
    ),
    ui.hr(),
    ui.br(),
    header("input"),
    ui.br(),
    ui.input("name:", model.name, style.Px(50), SetName),
    ui.br(),
    ui.text("hello " <> model.name),
    ui.br(),
    header("input hidden"),
    ui.br(),
    ui.input_hidden("secret:", model.secret, style.Px(50), SetSecret),
    ui.br(),
    ui.text("secret " <> model.secret),
    ui.br(),
    header("table"),
    ui.br(),
    ui.table(style.Px(50), model.csv),
    ui.br(),
    header("table key value"),
    ui.br(),
    ui.table_kv(style.Px(50), model.kv),
    ui.br(),
    header("image"),
    ui.br(),
    ui.image_unstable(pic),
    ui.br(),
    header("text"),
    ui.br(),
    ui.text("the quick brown fox jumped over the lazy dog"),
    ui.br(),
    header("text wrapped"),
    ui.br(),
    ui.text_wrapped(
      "The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog. 

The quick brown fox jumped over the lazy dog. The quick brown fox jumped over the lazy dog.",
    ),
    ui.br(),
    header("progress"),
    ui.br(),
    ui.progress(style.Px(50), 100, model.counter, style.Blue),
    ui.br(),
    header("colours"),
    ui.br(),
    model.counter
      |> int.to_string
      |> ui.text_styled(Some(style.Black), Some(style.White)),
    model.counter |> int.to_string |> ui.text_styled(Some(style.Red), None),
    model.counter |> int.to_string |> ui.text_styled(Some(style.Green), None),
    model.counter |> int.to_string |> ui.text_styled(Some(style.Yellow), None),
    model.counter |> int.to_string |> ui.text_styled(Some(style.Blue), None),
    model.counter |> int.to_string |> ui.text_styled(Some(style.Magenta), None),
    model.counter |> int.to_string |> ui.text_styled(Some(style.Cyan), None),
    model.counter |> int.to_string |> ui.text_styled(Some(style.White), None),
    ui.br(),
    header("buttons"),
    ui.br(),
    ui.row([
      ui.button("i: Increment", key.Char("i"), Increment),
      ui.button("d: Decrement", key.Char("d"), Decrement),
      ui.button_styled(
        "r: Reset",
        key.Char("r"),
        Reset,
        Some(style.Black),
        Some(style.Magenta),
        Some(style.Black),
        Some(style.Green),
      ),
      ui.button_styled(
        "a: Reset Async",
        key.Char("a"),
        SendReset,
        Some(style.Black),
        Some(style.Magenta),
        Some(style.Black),
        Some(style.Green),
      ),
    ]),
    ui.br(),
    header("graph"),
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
    ui.br(),
  ])
}

fn header(string: String) -> shore.Node(msg) {
  ui.text_styled(string, Some(style.Black), Some(style.Cyan))
}

// CMDS

fn reset() -> Msg {
  process.sleep(1000)
  Reset
}

const pic = "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAIAQMAAAD+wSzIAAAABlBMVEX///+/v7+jQ3Y5AAAADklEQVQI12P4AIX8EAgALgAD/aNpbtEAAAAASUVORK5CYII"
