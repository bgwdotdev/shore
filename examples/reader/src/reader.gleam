import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import shore
import simplifile

// MAIN

pub fn main() {
  let _ = shore.Spec(init:, update:, view:) |> shore.start
  process.sleep_forever()
}

// MODEL

type Model {
  Model(
    errors: List(Err),
    work_dir: String,
    files: List(File),
    content: String,
    focused: Int,
  )
}

fn init() -> #(Model, List(fn() -> Msg)) {
  let model =
    Model(errors: [], work_dir: "", files: [], content: "", focused: 0)
  let cmds = [pwd]
  #(model, cmds)
}

type File {
  File(name: String, info: simplifile.FileInfo)
}

// UPDATE

type Msg {
  NoOp
  DismissErrors
  Pwd(Result(String, Err))
  GetFiles(Result(List(String), Err))
  GetFileInfo(Result(File, Err))
  ReadFile(Result(String, Err))
  Up
  Down
}

fn update(model: Model, msg: Msg) -> #(Model, List(fn() -> Msg)) {
  case msg {
    NoOp -> #(model, [])
    DismissErrors -> #(Model(..model, errors: []), [])

    Pwd(Ok(work_dir)) -> #(Model(..model, work_dir:), [read_directory(work_dir)])
    Pwd(Error(e)) -> #(append_err(e, model), [])

    GetFiles(Ok(files)) -> #(model, files |> list.map(file_info))
    GetFiles(Error(e)) -> #(append_err(e, model), [])

    GetFileInfo(Ok(file)) -> #(Model(..model, files: [file, ..model.files]), [])
    GetFileInfo(Error(e)) -> #(append_err(e, model), [])

    ReadFile(Ok(content)) -> #(Model(..model, content:), [])
    ReadFile(Error(e)) -> #(append_err(e, model), [])

    Up -> {
      let model = Model(..model, focused: int.max(model.focused - 1, 0))
      let cmds = [update_read(model)]
      #(model, cmds)
    }
    Down -> {
      let model =
        Model(
          ..model,
          focused: int.min(model.focused + 1, list.length(model.files) - 1),
        )
      let cmds = [update_read(model)]
      #(model, cmds)
    }
  }
}

fn append_err(err: Err, model: Model) -> Model {
  Model(..model, errors: [err, ..model.errors])
}

fn update_read(model: Model) -> fn() -> Msg {
  let files =
    model.files
    |> list.sort(fn(a, b) {
      string.compare(string.lowercase(a.name), string.lowercase(b.name))
    })
  fn() { do_update_read(files, model.focused, 0) |> read }
}

fn do_update_read(files: List(File), focused: Int, idx: Int) -> File {
  case files {
    [] ->
      panic as "somehow got out of bounds, you fool! If only you handled this error!"
    [x, ..xs] ->
      case idx == focused {
        True -> x
        False -> do_update_read(xs, focused, idx + 1)
      }
  }
}

// VIEW

fn view(model: Model) -> shore.Node(Msg) {
  shore.Div(
    [
      shore.KeyBind("k", Up),
      shore.KeyBind("j", Down),
      model.work_dir |> shore.Text(None, None),
      shore.HR,
      shore.Div(
        [
          shore.Box(
            model.files
              |> list.sort(fn(a, b) {
                string.compare(
                  string.lowercase(a.name),
                  string.lowercase(b.name),
                )
              })
              |> list.index_map(fn(f, i) { view_file(f, i, model.focused) }),
            70,
            list.length(model.files),
            Some("file"),
          ),
          shore.Box(
            [model.content |> shore.TextMulti(None, None)],
            70,
            20,
            Some("preview"),
          ),
        ],
        shore.Row,
      ),
    ],
    shore.Col,
  )
}

fn view_file(file: File, idx: Int, focused: Int) -> shore.Node(msg) {
  let color = case file.info.mode |> int.digits(10) {
    Ok([1, ..]) -> Some(shore.Blue)
    Ok([3, ..]) -> Some(shore.White)
    _ -> Some(shore.Magenta)
  }
  let #(fg, bg) = case idx == focused {
    True -> #(Some(shore.Black), color)
    False -> #(color, None)
  }

  file.name |> shore.Text(fg, bg)
}

// CMD

fn pwd() -> Msg {
  simplifile.current_directory() |> result.map_error(ErrSimplifile) |> Pwd
}

fn read_directory(dir: String) -> fn() -> Msg {
  fn() {
    dir
    |> simplifile.read_directory
    |> result.map_error(ErrSimplifile)
    |> GetFiles
  }
}

fn file_info(item: String) -> fn() -> Msg {
  fn() {
    item
    |> simplifile.file_info
    |> result.map_error(ErrSimplifile)
    |> result.map(File(name: item, info: _))
    |> GetFileInfo
  }
}

fn read(file: File) -> Msg {
  file.name
  |> simplifile.read
  |> result.map_error(ErrSimplifile)
  |> ReadFile
}

// ERROR

type Err {
  ErrSimplifile(simplifile.FileError)
}
