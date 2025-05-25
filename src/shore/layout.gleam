import shore/internal
import shore/style

pub type Cell(msg) =
  internal.Cell(msg)

/// A Cell is an item within a Grid. Use Cells to define how many rows and
/// columns the content should cover.
///
pub fn cell(
  content content: internal.Node(msg),
  row row: #(Int, Int),
  col col: #(Int, Int),
) -> Cell(msg) {
  internal.Cell(content:, row:, col:)
}

/// A grid-based layout defining rows and columns which contain cells and the gaps between them.
///
/// This should be remeniscent of CSS Grid. You define a list of rows and
/// columns by size, then use Cells to fill the rows/columns to create descrete
/// areas of ui elements.
///
/// Consider using some of the default provided layouts, such as
/// `layout_center` and `layout_split` or view the examples/layouts for more
/// complex custom layouts.
///
/// Note: Layouts can be nested as long as it is the only child of a cell.
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
