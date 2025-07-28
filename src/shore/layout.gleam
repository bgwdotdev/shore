import shore/internal
import shore/style

/// A container item in a Grid
pub type Cell(msg) =
  internal.Cell(msg)

/// A Cell is a container item within a Grid which contains a collection of ui items (or another Grid).
///
/// A Cell can be a single "square" within a Grid, or it can span across multiple "squares" in a Grid.
///
/// Using the row and col arguments to define the start and end rows and columns
/// respectively that the content should span across (this is a 0-based index).
///
/// For example, in a Grid made up of four equally sized "squares" defined as
/// ```gleam
/// rows: [style.Pct(50), style.Pct(50)],
/// cols: [style.Pct(50), style.Pct(50)],
/// ```
/// (a.k.a. two rows and two columns at 50% each).
///
/// Then to position a cell, consider the following examples:
///
/// * `row: #(0,0), col: #(0,0)` would be the top left
/// * `row: #(0,1), col: #(0,0)` would be the left half
/// * `row: #(0,1), col: #(0,1)` would be the entire grid
/// * `row: #(0,0), col: #(0,1)` would be the top half
///
pub fn cell(
  content content: internal.Node(msg),
  row row: #(Int, Int),
  col col: #(Int, Int),
) -> Cell(msg) {
  internal.Cell(content:, row:, col:)
}

/// A grid-based layout defining rows and columns which contain cells and the
/// gaps between them.
///
/// This should be remeniscent of CSS Grid. You define a list of rows and
/// columns by size, then use Cells to fill the rows/columns to create descrete
/// areas of ui elements.
///
/// Consider using some of the default provided layouts, such as `center` and
/// `split` or view the examples/layouts for more complex custom layouts.
///
/// Note: Layouts can be nested as long as it is the only child of a cell.
///
pub fn grid(
  gap gap: Int,
  rows rows: List(style.Size),
  cols cols: List(style.Size),
  cells cells: List(internal.Cell(msg)),
) -> internal.Node(msg) {
  internal.Layouts(internal.Grid(gap:, rows:, columns: cols, cells:))
}

/// A layout which centers vertically and horizontally
pub fn center(
  content: internal.Node(msg),
  width: style.Size,
  height: style.Size,
) -> internal.Node(msg) {
  internal.Grid(
    gap: 0,
    rows: [style.Fill, height, style.Fill],
    columns: [style.Fill, width, style.Fill],
    cells: [internal.Cell(content:, row: #(1, 1), col: #(1, 1))],
  )
  |> internal.Layouts
}

/// A layout which has a 50/50 split
pub fn split(
  left: internal.Node(msg),
  right: internal.Node(msg),
) -> internal.Node(msg) {
  internal.Grid(
    gap: 0,
    rows: [style.Pct(100)],
    columns: [style.Pct(50), style.Fill],
    cells: [
      internal.Cell(content: left, row: #(0, 0), col: #(0, 0)),
      internal.Cell(content: right, row: #(0, 0), col: #(1, 1)),
    ],
  )
  |> internal.Layouts
}
