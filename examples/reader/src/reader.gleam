import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/task
import gleam/result
import gleam/string
import shore
import shore/key
import simplifile

// MAIN

pub fn main() {
  let exit = process.new_subject()
  let _ =
    shore.Spec(init:, update:, view:, exit:, keybinds: shore.default_keybinds())
    |> shore.start
  process.receive_forever(exit)
}

// MODEL

type Model {
  Model(
    errors: List(Err),
    root_dir: String,
    work_dir: List(String),
    files: List(File),
    content: String,
    focused: Int,
    file_count: Int,
  )
}

fn init() -> #(Model, List(fn() -> Msg)) {
  let model =
    Model(
      errors: [],
      root_dir: "",
      work_dir: [],
      files: [],
      content: "",
      focused: 0,
      file_count: 0,
    )
  let cmds = [pwd]
  #(model, cmds)
}

type File {
  File(
    name: String,
    info: simplifile.FileInfo,
    type_: simplifile.FileType,
    path: String,
  )
}

// UPDATE

type Msg {
  NoOp
  DismissErrors
  Pwd(Result(String, Err))
  GetFiles(Result(List(String), Err))
  GetFilesInfo(Result(List(File), Err))
  ReadFile(Result(String, Err))
  Up
  Down
  Open
  Back
}

fn update(model: Model, msg: Msg) -> #(Model, List(fn() -> Msg)) {
  case msg {
    NoOp -> #(model, [])
    DismissErrors -> #(Model(..model, errors: []), [])

    Pwd(Ok(dir)) -> #(
      Model(
        ..model,
        root_dir: dir,
        work_dir: string.split(dir, "/")
          |> list.map(fn(dir) {
            case dir {
              "" -> "/"
              x -> x
            }
          })
          |> list.reverse,
      ),
      [read_directory(dir)],
    )
    Pwd(Error(e)) -> #(append_err(e, model), [])

    GetFiles(Ok(files)) -> #(Model(..model, file_count: list.length(files)), [
      files_info(files, model.work_dir),
    ])
    GetFiles(Error(e)) -> #(append_err(e, model), [])

    GetFilesInfo(Ok(files)) -> {
      let model = Model(..model, files:)
      #(model, [update_read(model)])
    }
    GetFilesInfo(Error(e)) -> #(append_err(e, model), [])

    ReadFile(Ok(content)) -> #(Model(..model, content:), [])
    ReadFile(Error(ErrSimplifile(e, _))) -> #(
      Model(..model, content: "error: " <> simplifile.describe_error(e)),
      [],
    )
    ReadFile(Error(_)) -> #(model, [])

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
    Open -> {
      let file =
        model.files
        |> sort_files
        |> do_update_read(model.focused, 0)
      case file.type_ {
        simplifile.Directory | simplifile.Symlink -> {
          let work_dir = [file.name, ..model.work_dir]
          #(Model(..model, work_dir:, files: [], focused: 0), [
            relative_path(file.name, model.work_dir) |> read_directory(),
          ])
        }
        simplifile.File | simplifile.Other -> #(model, [])
      }
    }
    Back -> {
      let work_dir = case model.work_dir {
        ["/"] -> model.work_dir
        _ -> model.work_dir |> list.drop(1)
      }
      let cmds = [read_directory(work_dir |> list.reverse |> string.join("/"))]
      #(Model(..model, work_dir:, files: [], focused: 0), cmds)
    }
  }
}

fn append_err(err: Err, model: Model) -> Model {
  Model(..model, errors: [err, ..model.errors])
}

fn relative_path(file: String, work_dir: List(String)) -> String {
  [file, ..work_dir] |> list.reverse |> string.join("/")
}

fn sort_files(files: List(File)) -> List(File) {
  list.sort(files, fn(a, b) {
    string.compare(string.lowercase(a.name), string.lowercase(b.name))
  })
}

fn update_read(model: Model) -> fn() -> Msg {
  let files = model.files |> sort_files
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
  case model.errors {
    [] -> {
      shore.Split(shore.Split2(
        shore.Horizontal,
        shore.Ratio2(2, 98),
        shore.Split1(shore.Div(
          [
            shore.KeyBind(key.Char("k"), Up),
            shore.KeyBind(key.Char("j"), Down),
            shore.KeyBind(key.Char("l"), Open),
            shore.KeyBind(key.Char("h"), Back),
            model.work_dir
              |> list.reverse
              |> string.join("/")
              |> string.replace("//", "/")
              |> shore.Text(None, None),
            shore.HR,
          ],
          shore.Col,
        )),
        shore.Split2(
          shore.Vertical,
          shore.Ratio2(50, 50),
          shore.Split1(shore.Box(
            model.files
              |> sort_files
              |> list.index_map(fn(f, i) { view_file(f, i, model.focused) }),
            Some("files"),
          )),
          shore.Split1(shore.Box(
            [model.content |> shore.TextMulti(None, None)],
            Some("preview"),
          )),
        ),
      ))
    }
    xs ->
      shore.Div(
        [
          shore.KeyBind(key.Char("q"), DismissErrors),
          xs
            |> list.map(string.inspect)
            |> string.join("\n")
            |> shore.TextMulti(Some(shore.Black), Some(shore.Red)),
          model
            |> string.inspect
            |> string.replace("),", ")\n")
            |> shore.TextMulti(None, None),
        ],
        shore.Col,
      )
  }
}

fn view_file(file: File, idx: Int, focused: Int) -> shore.Node(msg) {
  let color = case file.type_ {
    simplifile.Directory -> Some(shore.Blue)
    simplifile.File -> Some(shore.White)
    simplifile.Symlink -> Some(shore.Red)
    simplifile.Other -> Some(shore.Yellow)
  }
  let #(fg, bg) = case idx == focused {
    True -> #(Some(shore.Black), color)
    False -> #(color, None)
  }
  file.name |> shore.Text(fg, bg)
}

// CMD

fn pwd() -> Msg {
  simplifile.current_directory()
  |> result.map_error(ErrSimplifile(_, "$PWD"))
  |> Pwd
}

fn read_directory(dir: String) -> fn() -> Msg {
  fn() {
    dir
    |> simplifile.read_directory
    |> result.map_error(ErrSimplifile(_, dir))
    |> GetFiles
  }
}

fn files_info(items: List(String), work_dir: List(String)) -> fn() -> Msg {
  fn() {
    list.map(items, fn(item) {
      fn() {
        let path = item |> relative_path(work_dir)
        simplifile.file_info(path)
        |> result.try_recover(fn(_) { simplifile.link_info(path) })
        |> result.map_error(ErrSimplifile(_, item))
        |> result.map(fn(info) {
          File(name: item, info:, type_: simplifile.file_info_type(info), path:)
        })
      }
      |> task.async
    })
    |> task.try_await_all(100)
    |> list.map(result.map_error(_, ErrTask))
    |> list.map(result.flatten)
    |> result.all
    |> GetFilesInfo
  }
}

fn read(file: File) -> Msg {
  case file.type_ {
    simplifile.File ->
      file.path
      |> simplifile.read
      |> result.map_error(ErrSimplifile(_, file.name))
      |> ReadFile

    simplifile.Directory ->
      file.path
      |> simplifile.read_directory
      |> result.map_error(ErrSimplifile(_, file.name))
      |> result.map(string.join(_, "\n"))
      |> ReadFile

    simplifile.Symlink ->
      file.path
      |> simplifile.read_directory
      |> result.map_error(ErrSimplifile(_, file.name))
      |> result.map(string.join(_, "\n"))
      |> ReadFile

    simplifile.Other ->
      file.path
      |> simplifile.read
      |> result.map_error(ErrSimplifile(_, file.name))
      |> ReadFile
  }
}

// ERROR

type Err {
  ErrSimplifile(error: simplifile.FileError, file: String)
  ErrTask(error: task.AwaitError)
}
