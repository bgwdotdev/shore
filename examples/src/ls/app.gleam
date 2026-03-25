import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import shore
import shore/layout
import shore/style
import shore/ui
import simplifile

// MAIN

pub fn main() {
  let model = init()
  let height = { model.files |> list.length } + 1 |> style.Px
  shore.static(model, view, style.Fill, height) |> io.println
}

// ARGS

fn args() -> String {
  let args = get_plain_arguments() |> list.map(charlist_to_string)
  case args {
    [path, ..] -> path
    [] -> "."
  }
}

// MODEL

type Model {
  Model(files: List(File))
}

type File {
  File(name: String, info: simplifile.FileInfo)
}

fn init() -> Model {
  let path = args()
  let files = path |> directory |> list.map(file(_, path))
  Model(files:)
}

fn directory(path: String) -> List(String) {
  let assert Ok(files) = simplifile.read_directory(path)
  files |> list.sort(string.compare)
}

fn file(file_name: String, path: String) -> File {
  let path = path <> "/" <> file_name
  let assert Ok(info) = simplifile.file_info(path)
  File(file_name, info)
}

// VIEW

fn view(model: Model) -> shore.Node(msg) {
  let gap = 0
  let rows =
    model.files
    |> list.map(fn(_) { style.Px(1) })
    |> list.prepend(style.Px(1))
  let cols = [style.Fill, style.Px(10), style.Fill]
  let cells =
    list.index_map(model.files, view_file)
    |> list.prepend(view_header())
    |> list.flatten
  layout.grid(gap:, rows:, cols:, cells:)
}

fn view_header() -> List(layout.Cell(msg)) {
  let header = fn(str, col) {
    str
    |> ui.text_styled(Some(style.Blue), None)
    |> layout.cell(#(0, 0), #(col, col))
  }
  [
    "name" |> header(0),
    "size" |> header(1),
    "modified" |> header(2),
  ]
}

fn view_file(file: File, idx: Int) -> List(layout.Cell(msg)) {
  let header_offset = 1
  let idx = idx + header_offset
  [
    file.name
      |> ui.text
      |> layout.cell(#(idx, idx), #(0, 0)),
    file.info.size
      |> int.to_string
      |> ui.text
      |> layout.cell(#(idx, idx), #(1, 1)),
    file.info.mtime_seconds
      |> date
      |> ui.text
      |> layout.cell(#(idx, idx), #(2, 2)),
  ]
}

// HELPER

fn date(time: Int) -> String {
  time
  |> system_time_to_rfc3339([Return(Binary)])
  |> string.drop_end(15)
}

// FFI

@external(erlang, "calendar", "system_time_to_rfc3339")
fn system_time_to_rfc3339(time: Int, options: List(TimeOption)) -> String

type TimeOption {
  Binary
  Return(TimeOption)
}

type Charlist

@external(erlang, "init", "get_plain_arguments")
fn get_plain_arguments() -> List(Charlist)

@external(erlang, "unicode", "characters_to_binary")
fn charlist_to_string(charlist: Charlist) -> String
