import gleam/option.{None}
import gleeunit
import gleeunit/should
import shore

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  1
  |> should.equal(1)
}

fn content() -> shore.Node(Nil) {
  shore.Debug
}

pub fn layout_1_test() {
  let width = 50
  let height = 50
  let rows = [shore.Px(10), shore.Fill]
  let columns = [shore.Pct(50), shore.Fill]
  let cells = [shore.Cell(content:, row: #(0, 0), col: #(0, 0))]
  let layout =
    shore.Grid(rows:, columns:, cells:) |> shore.layout(width, height)
  layout |> echo
  should.equal(True, True)
}

pub fn layout_2_test() {
  let width = 50
  let height = 50
  let rows = [shore.Px(10), shore.Fill]
  let columns = [shore.Pct(50), shore.Fill]
  let cells = [
    shore.Cell(content:, row: #(0, 0), col: #(0, 0)),
    shore.Cell(content:, row: #(1, 1), col: #(1, 1)),
  ]
  let layout =
    shore.Grid(rows:, columns:, cells:) |> shore.layout(width, height)
  layout |> echo
  should.equal(True, True)
}

pub fn layout_3_test() {
  let width = 50
  let height = 50
  let rows = [shore.Px(10), shore.Fill]
  let columns = [shore.Pct(50), shore.Fill]
  let cells = [
    shore.Cell(content:, row: #(0, 0), col: #(0, 1)),
    shore.Cell(content:, row: #(1, 1), col: #(0, 1)),
  ]
  let layout =
    shore.Grid(rows:, columns:, cells:) |> shore.layout(width, height)
  layout |> echo
  should.equal(True, True)
}

pub fn layout_4_test() {
  let width = 50
  let height = 50
  let rows = [shore.Pct(50), shore.Fill]
  let columns = [shore.Pct(50), shore.Fill]
  let cells = [
    shore.Cell(content:, row: #(0, 1), col: #(0, 0)),
    shore.Cell(content:, row: #(0, 1), col: #(1, 1)),
  ]
  let layout =
    shore.Grid(rows:, columns:, cells:) |> shore.layout(width, height)
  layout |> echo
  should.equal(True, True)
}

pub fn layout_5_test() {
  shore.Grid(
    rows: [shore.Px(10), shore.Pct(50), shore.Px(20)],
    columns: [shore.Pct(50), shore.Pct(50)],
    cells: [
      shore.Cell(
        //content: fn() { shore.Text("counter app", None, None) },
        //content: fn() { shore.Debug },
        content: fn() { shore.Box([shore.Debug], None) },
        row: #(0, 0),
        col: #(0, 0),
      ),
      shore.Cell(
        //content: fn() { shore.Debug },
        content: fn() { shore.Box([shore.Debug], None) },
        //content: fn() { view_body(model) },
        row: #(1, 1),
        col: #(0, 0),
      ),
      shore.Cell(
        //content: fn() { shore.Text("keybinds", None, None) },
        //content: fn() { shore.Debug },
        content: fn() { shore.Box([shore.Debug], None) },
        row: #(2, 2),
        col: #(0, 0),
      ),
    ],
  )
  |> shore.layout(50, 50)
  |> echo
  should.equal(True, True)
}
