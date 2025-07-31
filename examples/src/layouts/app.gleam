import gleam/erlang/process
import gleam/int
import gleam/option.{None, Some}
import shore
import shore/key
import shore/layout
import shore/style
import shore/ui

// MAIN

pub fn main() {
  let exit = process.new_subject()
  let assert Ok(_actor) =
    shore.spec(
      init:,
      update:,
      view: view,
      exit:,
      keybinds: shore.default_keybinds(),
      redraw: shore.on_timer(16),
    )
    |> shore.start
  exit |> process.receive_forever
}

// MODEL

type Model {
  Model(
    page: Page,
    counter: Int,
    username: String,
    password: String,
    login: Bool,
  )
}

type Page {
  Select
  One
  Two
  Three
  Four
}

fn init() -> #(Model, List(fn() -> Msg)) {
  let model =
    Model(page: Select, counter: 0, username: "", password: "", login: False)
  let cmds = []
  #(model, cmds)
}

// UPDATE

type Msg {
  NoOp
  SetPage(Page)
  Increment
  Decrement
  SetUsername(String)
  SetPassword(String)
  SubmitLogin
}

fn update(model: Model, msg: Msg) -> #(Model, List(fn() -> Msg)) {
  case msg {
    NoOp -> #(model, [])
    SetPage(page) -> #(Model(..model, page:), [])
    Increment -> #(Model(..model, counter: int.min(100, model.counter + 1)), [])
    Decrement -> #(Model(..model, counter: int.max(0, model.counter - 1)), [])
    SetUsername(username) -> #(Model(..model, username:), [])
    SetPassword(password) -> #(Model(..model, password:), [])
    SubmitLogin -> #(
      Model(..model, username: "", password: "", login: True),
      [],
    )
  }
}

// VIEW

fn view(model: Model) -> shore.Node(Msg) {
  case model.page {
    Select -> view_select(model)
    One -> view_1(model)
    Two -> view_2(model)
    Three -> view_3(model)
    Four -> view_4(model)
  }
}

fn view_select(_model: Model) -> shore.Node(Msg) {
  [
    "Welcome!

Press the number to navigate to the example.

Press escape to return to this menu.

Press crlt+x to quit.

1: Many zones
2: Simple login
3: Simple split
4: Counter
"
      |> ui.text,
    ui.keybind(key.Char("1"), SetPage(One)),
    ui.keybind(key.Char("2"), SetPage(Two)),
    ui.keybind(key.Char("3"), SetPage(Three)),
    ui.keybind(key.Char("4"), SetPage(Four)),
  ]
  |> ui.box(None)
  |> ui.align(style.Left, _)
  |> layout.center(style.Px(50), style.Px(14))
}

fn key_select() -> shore.Node(Msg) {
  ui.keybind(key.Esc, SetPage(Select))
}

fn view_1(_model: Model) -> shore.Node(Msg) {
  let content = ui.box([ui.debug(), key_select()], None)
  layout.grid(
    gap: 1,
    rows: [style.Px(3), style.Fill, style.Px(20), style.Px(10)],
    cols: [style.Pct(33), style.Fill, style.Fill],
    cells: [
      layout.cell(content:, row: #(0, 0), col: #(1, 2)),
      layout.cell(content:, row: #(1, 1), col: #(0, 1)),
      layout.cell(content:, row: #(1, 1), col: #(2, 2)),
      layout.cell(content:, row: #(2, 2), col: #(0, 0)),
      layout.cell(content:, row: #(2, 2), col: #(1, 1)),
      layout.cell(content:, row: #(2, 2), col: #(2, 2)),
      layout.cell(content:, row: #(3, 3), col: #(0, 1)),
    ],
  )
}

fn view_2(model: Model) -> shore.Node(Msg) {
  let username =
    ui.input(
      "username:",
      model.username,
      //shore.Px(20),
      style.Fill,
      SetUsername,
    )
  let password =
    ui.input_hidden("password:", model.password, style.Fill, SetPassword)
  layout.center(
    ui.box(
      [
        username,
        password,
        ui.br(),
        ui.button("s: submit", key.Char("s"), SubmitLogin),
        key_select(),
      ],
      Some("login"),
    ),
    style.Px(50),
    style.Px(6),
  )
}

fn view_3(_model: Model) -> shore.Node(Msg) {
  layout.split(
    ui.box(
      [ui.text("username"), ui.text("password"), key_select()],
      Some("left"),
    ),
    ui.box([ui.text("username"), ui.text("password")], Some("right")),
  )
}

fn view_4(model: Model) -> shore.Node(Msg) {
  model
  |> view_body
  |> ui.box(Some("Counter"))
  |> ui.align(style.Right, _)
  |> layout.center(style.Px(55), style.Px(8))
}

fn view_body(model: Model) -> List(shore.Node(Msg)) {
  let color = case model.counter {
    x if x < 25 -> style.Red
    x if x < 50 -> style.Cyan
    x if x < 75 -> style.Yellow
    x if x <= 100 -> style.Green
    _ -> style.White
  }
  [
    ui.text_styled(int.to_string(model.counter), Some(color), None),
    ui.br(),
    ui.progress(style.Pct(30), 100, model.counter, color),
    ui.br(),
    ui.row([
      ui.button("i: increment", key.Char("i"), Increment),
      ui.button("d: decrement", key.Char("d"), Decrement),
    ]),
    key_select(),
  ]
}
