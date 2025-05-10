import gleam/erlang/process
import gleam/int
import gleam/option.{None, Some}
import shore
import shore/key

// MAIN

pub fn main() {
  let exit = process.new_subject()
  let assert Ok(_actor) =
    shore.Spec(
      init:,
      update:,
      view: view,
      exit:,
      keybinds: shore.default_keybinds(),
      redraw: shore.OnTimer(16),
    )
    |> shore.start
  exit |> process.receive_forever
}

// MODEL

pub opaque type Model {
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

pub opaque type Msg {
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

fn view_select(model: Model) -> shore.Node(Msg) {
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
      |> shore.TextMulti(None, None),
    shore.KeyBind(key.Char("1"), SetPage(One)),
    shore.KeyBind(key.Char("2"), SetPage(Two)),
    shore.KeyBind(key.Char("3"), SetPage(Three)),
    shore.KeyBind(key.Char("4"), SetPage(Four)),
  ]
  |> shore.Box(None)
  |> shore.Aligned(shore.Left, _)
  |> shore.layout_center(shore.Px(50), shore.Px(14))
  |> shore.Layouts
}

fn key_select() -> shore.Node(Msg) {
  shore.KeyBind(key.Esc, SetPage(Select))
}

fn view_1(model: Model) -> shore.Node(Msg) {
  //let content = fn() { shore.Debug }
  let content = shore.Box([shore.Debug, key_select()], None)
  shore.Grid(
    gap: 1,
    rows: [shore.Px(3), shore.Fill, shore.Px(20), shore.Px(10)],
    columns: [shore.Pct(33), shore.Fill, shore.Fill],
    cells: [
      shore.Cell(content:, row: #(0, 0), col: #(1, 2)),
      shore.Cell(content:, row: #(1, 1), col: #(0, 1)),
      shore.Cell(content:, row: #(1, 1), col: #(2, 2)),
      shore.Cell(content:, row: #(2, 2), col: #(0, 0)),
      shore.Cell(content:, row: #(2, 2), col: #(1, 1)),
      shore.Cell(content:, row: #(2, 2), col: #(2, 2)),
      shore.Cell(content:, row: #(3, 3), col: #(0, 1)),
    ],
  )
  |> shore.Layouts
}

fn view_2(model: Model) -> shore.Node(Msg) {
  let username =
    shore.Input(
      "username:",
      model.username,
      //shore.Px(20),
      shore.Fill,
      SetUsername,
      shore.Simple,
    )
  let password =
    shore.Input(
      "password:",
      model.password,
      shore.Fill,
      SetPassword,
      shore.Simple,
    )
  shore.Layouts(shore.layout_center(
    shore.Box(
      [
        username,
        password,
        shore.BR,
        shore.Button("s: submit", key.Char("s"), SubmitLogin),
        key_select(),
      ],
      Some("login"),
    ),
    shore.Px(50),
    shore.Px(6),
  ))
}

fn view_3(model: Model) -> shore.Node(Msg) {
  shore.Layouts(shore.layout_split(
    shore.Box(
      [
        shore.Text("username", None, None),
        shore.Text("password", None, None),
        key_select(),
      ],
      Some("left"),
    ),
    shore.Box(
      [shore.Text("username", None, None), shore.Text("password", None, None)],
      Some("right"),
    ),
  ))
}

fn view_4(model: Model) -> shore.Node(Msg) {
  model
  |> view_body
  |> shore.Box(Some("Counter"))
  |> shore.Aligned(shore.Right, _)
  |> shore.layout_center(shore.Px(55), shore.Px(8))
  |> shore.Layouts
}

fn view_body(model: Model) -> List(shore.Node(Msg)) {
  let color = case model.counter {
    x if x < 25 -> shore.Red
    x if x < 50 -> shore.Cyan
    x if x < 75 -> shore.Yellow
    x if x <= 100 -> shore.Green
    _ -> shore.White
  }
  [
    shore.Text(int.to_string(model.counter), Some(color), None),
    shore.BR,
    shore.Progress(shore.Pct(30), 100, model.counter, color),
    shore.BR,
    shore.DivRow([
      //shore.Debug,
      //shore.Debug,
      shore.Button("i: increment", key.Char("i"), Increment),
      shore.Button("d: decrement", key.Char("d"), Decrement),
    ]),
    key_select(),
  ]
}
