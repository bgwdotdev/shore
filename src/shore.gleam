import gleam/erlang/charlist.{type Charlist}
import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/pair
import gleam/result
import gleam/string
import shore/key.{type Key}

//
// LAYOUT
//

pub type Layout(msg) {
  Grid(
    gap: Int,
    rows: List(Ratio),
    columns: List(Ratio),
    cells: List(Cell(msg)),
  )
}

pub type Cell(msg) {
  Cell(content: Node(msg), row: #(Int, Int), col: #(Int, Int))
}

fn layout(
  layout: Layout(msg),
  width: Int,
  height: Int,
) -> List(#(Node(msg), Pos)) {
  let col_sizes = layout.columns |> calc_sizes(width, _)
  let row_sizes = layout.rows |> calc_sizes(height, _)
  layout.cells
  |> list.map(fn(cell) {
    let #(x, w) = calc_cell_size(layout.gap, cell.col.0, cell.col.1, col_sizes)
    let #(y, h) = calc_cell_size(layout.gap, cell.row.0, cell.row.1, row_sizes)
    #(cell.content, Pos(x:, y:, width: w, height: h))
  })
}

fn calc_cell_size(gap: Int, from: Int, to: Int, of: List(Int)) {
  list.index_fold(of, #(1, 0), fn(acc, item, idx) {
    case idx {
      x if x >= from && x <= to -> #(acc.0, acc.1 + item)
      x if x == from - 1 -> #(acc.0 + gap + item, acc.1 - gap)
      x if x < from -> #(acc.0 + item, acc.1)
      _ -> acc
    }
  })
}

fn calc_sizes(max: Int, sizes: List(Ratio)) {
  let first =
    list.map(sizes, fn(size) {
      case size {
        Px(px) -> Some(px)
        Pct(pct) -> Some(max * pct / 100)
        Fill -> None
      }
    })
  let total_known_size =
    list.fold(first, 0, fn(acc, i) {
      case i {
        Some(px) -> acc + px
        None -> acc
      }
    })
  let total_unknown_count = first |> list.filter(option.is_none) |> list.length
  let remainder = max - total_known_size
  let remainder_split = remainder / total_unknown_count
  let round_up = remainder - remainder_split * total_unknown_count
  do_calc_sizes(first, remainder_split, round_up, [])
}

fn do_calc_sizes(
  sizes: List(Option(Int)),
  remainder_split: Int,
  round_up: Int,
  acc: List(Int),
) -> List(Int) {
  case sizes {
    [] -> list.reverse(acc)
    [x, ..xs] ->
      case x {
        Some(px) ->
          [px, ..acc] |> do_calc_sizes(xs, remainder_split, round_up, _)
        None ->
          [remainder_split + round_up, ..acc]
          |> do_calc_sizes(xs, remainder_split, 0, _)
      }
  }
}

//
// LAYOUT HELPER
//

pub fn layout_center(content: Node(msg)) -> Layout(msg) {
  Grid(
    gap: 0,
    rows: [Fill, Px(25), Fill],
    columns: [Fill, Px(50), Fill],
    cells: [Cell(content:, row: #(1, 1), col: #(1, 1))],
  )
}

pub fn layout_split(left: Node(msg), right: Node(msg)) -> Layout(msg) {
  Grid(gap: 0, rows: [Pct(100)], columns: [Pct(50), Fill], cells: [
    Cell(content: left, row: #(0, 0), col: #(0, 0)),
    Cell(content: right, row: #(0, 0), col: #(1, 1)),
  ])
}

//
// INIT
//

pub type Spec(model, msg) {
  Spec(
    init: fn() -> #(model, List(fn() -> msg)),
    view: fn(model) -> Node(msg),
    update: fn(model, msg) -> #(model, List(fn() -> msg)),
    exit: process.Subject(Nil),
    keybinds: Keybinds,
    redraw: Redraw,
  )
}

type State(model, msg) {
  State(
    spec: Spec(model, msg),
    model: model,
    width: Int,
    height: Int,
    tasks: process.Subject(Event(msg)),
    mode: Mode,
    last_input: String,
    focused: Option(Focused(msg)),
  )
}

pub type Keybinds {
  Keybinds(
    exit: Key,
    focus_clear: Key,
    focus_next: Key,
    focus_prev: Key,
    mode_insert: Key,
    mode_normal: Key,
  )
}

pub fn default_keybinds() -> Keybinds {
  Keybinds(
    exit: key.Ctrl("X"),
    focus_clear: key.Esc,
    focus_next: key.Tab,
    focus_prev: key.BackTab,
    mode_insert: key.Enter,
    mode_normal: key.Esc,
  )
}

pub fn vim_keybinds() -> Keybinds {
  Keybinds(
    exit: key.Char("Q"),
    focus_clear: key.Esc,
    focus_next: key.Char("j"),
    focus_prev: key.Char("k"),
    mode_insert: key.Char("i"),
    mode_normal: key.Esc,
  )
}

type Focused(msg) {
  Focused(
    label: String,
    value: String,
    event: fn(String) -> msg,
    cursor: Int,
    offset: Int,
    width: Int,
  )
}

type Mode {
  Insert
  Normal
}

pub fn start(
  spec: Spec(model, msg),
) -> Result(Subject(Event(msg)), actor.StartError) {
  raw_erl()
  { c(HideCursor) <> c(AltBuffer) } |> io.print
  use shore <- result.map(shore_start(spec))
  process.start(fn() { read_input(shore, spec.keybinds.exit) }, True)
  redraw_on_timer(spec, shore)
  shore
}

fn shore_start(spec: Spec(model, msg)) {
  actor.Spec(
    init: fn() {
      let tasks = process.new_subject()
      let #(model, task_init) = spec.init()
      let assert Ok(width) = terminal_columns()
      let assert Ok(height) = terminal_rows()
      let state =
        State(
          spec:,
          model:,
          width:,
          height:,
          tasks:,
          mode: Normal,
          last_input: "",
          focused: None,
        )
      let _first_paint = model |> spec.view |> render(state, _, key.Null)
      let queue =
        process.new_selector() |> process.selecting(tasks, function.identity)
      task_init |> task_handler(tasks)
      actor.Ready(state, queue)
    },
    init_timeout: 1000,
    loop: shore_loop,
  )
  |> actor.start_spec
}

//
// EVENT
//

pub opaque type Event(msg) {
  KeyPress(Key)
  Cmd(msg)
  Redraw
  Exit
}

pub fn cmd(msg: msg) -> Event(msg) {
  Cmd(msg)
}

pub fn exit() -> Event(msg) {
  Exit
}

fn shore_loop(event: Event(msg), state: State(model, msg)) {
  case event {
    Cmd(msg) -> {
      let #(model, tasks) = state.spec.update(state.model, msg)
      tasks |> task_handler(state.tasks)
      let state = State(..state, model: model)
      let state = update_viewport(state)
      redraw_on_update(state, key.Null)
      actor.continue(state)
    }
    KeyPress(input) -> {
      case state.mode {
        Normal -> {
          let ui = state.spec.view(state.model)

          // check for framework key events
          let state = case control_event(input, state.spec.keybinds) {
            // TODO: decouple mode from focus clear?
            Some(FocusClear) -> State(..state, focused: None, mode: Normal)
            Some(FocusPrev) -> {
              let focusable = list_focusable([ui], state)
              let focused = case state.focused {
                None -> focusable |> list.first |> option.from_result
                Some(x) -> focusable |> focus_next(x, False)
              }
              State(..state, focused:)
            }
            Some(FocusNext) -> {
              let focusable = list_focusable([ui], state) |> list.reverse
              let focused = case state.focused {
                None -> focusable |> list.first |> option.from_result
                Some(x) -> focusable |> focus_next(x, False)
              }
              State(..state, focused:)
            }
            Some(ModeInsert) -> State(..state, mode: Insert)
            None -> state
          }

          // pass the key event onto the application as well
          let state = {
            let model = case detect_event(state, ui, input) {
              Some(msg) -> {
                let #(model, tasks) = state.spec.update(state.model, msg)
                tasks |> task_handler(state.tasks)
                model
              }
              None -> state.model
            }
            State(..state, model:)
          }

          // render
          let state = update_viewport(state)
          redraw_on_update(state, input)
          actor.continue(state)
        }
        Insert -> {
          let mode = case input == state.spec.keybinds.mode_normal {
            True -> Normal
            False -> Insert
          }
          let #(focused, model) = case state.focused {
            Some(focused) -> {
              let focused = input_handler(focused, input)
              let #(model, tasks) =
                state.spec.update(state.model, focused.event(focused.value))
              tasks |> task_handler(state.tasks)
              #(Some(focused), model)
            }
            x -> #(x, state.model)
          }
          let state = State(..state, focused:, mode:, model:)
          let state = update_viewport(state)
          redraw_on_update(state, input)
          actor.continue(state)
        }
      }
    }
    Redraw -> {
      let state = update_viewport(state)
      redraw(state, key.Null)
      actor.continue(state)
    }
    Exit -> {
      { c(ShowCursor) <> c(MainBuffer) } |> io.print
      process.send(state.spec.exit, Nil)
      actor.Stop(process.Normal)
    }
  }
}

fn update_viewport(state: State(model, msg)) -> State(model, msg) {
  let assert Ok(width) = terminal_columns()
  let assert Ok(height) = terminal_rows()
  State(..state, width:, height:)
}

// TDOO: fix to be tail call recursive?
fn detect_event(
  state: State(model, msg),
  node: Node(msg),
  input: Key,
) -> Option(msg) {
  case node {
    Input(..) -> None
    HR | HR2(..) | Bar(..) | BR | Progress(..) | Table(..) -> None
    Text(..) | TextMulti(..) -> None
    Button(key:, event:, ..) if input == key -> Some(event)
    Button(..) -> None
    KeyBind(key, event) if input == key -> Some(event)
    KeyBind(..) -> None
    Div(children, _) | Box(children, _) ->
      do_detect_event(state, children, input)
    Split(splits) ->
      case splits {
        Split2(a:, b:, ..) ->
          detect_event(state, Split(a), input)
          |> option.lazy_or(fn() { detect_event(state, Split(b), input) })
        Split1(node) -> detect_event(state, node, input)
      }
    Layouts(layout) ->
      layout.cells
      |> list.map(fn(cell) { detect_event(state, cell.content, input) })
      |> option.values
      |> list.first
      |> option.from_result
    Debug -> None
  }
}

fn do_detect_event(
  state: State(model, msg),
  children: List(Node(msg)),
  input: Key,
) -> Option(msg) {
  case children {
    [] -> None
    [x, ..xs] ->
      case detect_event(state, x, input) {
        Some(msg) -> Some(msg)
        None -> do_detect_event(state, xs, input)
      }
  }
}

//
// REDRAW
//

/// configures the behaviour which causes the ui to be redrawn
pub type Redraw {
  /// whenever a new event happens, the ui will update
  /// suitable for infrequently changing state
  OnUpdate
  /// every x milliseconds, trigger a redraw
  /// 17 for 60fps
  /// 33 for 30fps
  OnTimer(Int)
}

fn redraw_on_timer(spec: Spec(model, msg), shore: Subject(Event(msg))) {
  case spec.redraw {
    OnUpdate -> Nil
    OnTimer(x) -> {
      process.start(fn() { do_redraw_on_timer(shore, x) }, True)
      Nil
    }
  }
}

fn do_redraw_on_timer(shore: Subject(Event(msg)), x: Int) -> Nil {
  process.send(shore, Redraw)
  process.sleep(x)
  do_redraw_on_timer(shore, x)
}

fn redraw(state: State(model, msg), input: Key) -> Nil {
  state.model |> state.spec.view |> render(state, _, input)
}

fn redraw_on_update(state: State(model, msg), input: Key) -> Nil {
  case state.spec.redraw {
    OnUpdate -> redraw(state, input)
    OnTimer(_) -> Nil
  }
}

//
// TASKS
//

fn task_handler(tasks: List(fn() -> msg), queue: Subject(Event(msg))) -> Nil {
  list.each(tasks, fn(task) {
    fn() { task() |> Cmd |> process.send(queue, _) }
    |> process.start(False)
  })
}

//
// FIELD FOCUS
//

fn list_focusable(
  children: List(Node(msg)),
  state: State(model, msg),
) -> List(Focused(msg)) {
  let pos = Pos(0, 0, state.width, state.height)
  do_list_focusable(pos, children, [])
}

fn do_list_focusable(
  pos: Pos,
  children: List(Node(msg)),
  acc: List(Focused(msg)),
) -> List(Focused(msg)) {
  case children {
    [] -> acc
    [x, ..xs] ->
      case x {
        Div(children, _) ->
          do_list_focusable(pos, xs, do_list_focusable(pos, children, acc))
        Box(children, _) -> {
          let pos = Pos(..pos, width: pos.width - 4, height: pos.height - 2)
          do_list_focusable(pos, xs, do_list_focusable(pos, children, acc))
        }
        Layouts(l) -> {
          layout(l, pos.width, pos.height)
          |> list.map(fn(i) { do_list_focusable(i.1, [i.0], acc) })
          |> list.flatten
        }
        Split(split) -> {
          case split {
            Split1(child) ->
              do_list_focusable(pos, xs, do_list_focusable(pos, [child], acc))
            Split2(_, _, a, b) ->
              do_list_focusable(
                pos,
                xs,
                do_list_focusable(pos, [Split(a), Split(b)], acc),
              )
          }
        }
        Input(label, value, width, event, _) -> {
          let cursor = string.length(value)
          let width = calc_size_input(width, pos.width, label)
          let focused =
            Focused(label:, value:, event:, offset: 0, cursor:, width:)
          let offset = input_offset(cursor, focused.offset, focused.width)
          do_list_focusable(pos, xs, [Focused(..focused, offset:), ..acc])
        }
        _ -> do_list_focusable(pos, xs, acc)
      }
  }
}

fn focus_next(
  focusable: List(Focused(msg)),
  focused: Focused(msg),
  next: Bool,
) -> Option(Focused(msg)) {
  case focusable {
    [] -> None
    [x, ..xs] ->
      case next {
        True -> Some(x)
        False -> focus_next(xs, focused, x == focused)
      }
  }
}

//
// TEXT INPUT
//

fn input_handler(focused: Focused(msg), input: Key) -> Focused(msg) {
  case input {
    key.Backspace -> {
      let cursor = int.max(0, focused.cursor - 1)
      let offset = int.max(0, focused.offset - 1)
      Focused(
        ..focused,
        value: focused.value |> string_backspace(focused.cursor, -1),
        cursor:,
        offset:,
      )
    }
    //key.Up -> string.drop_end(value, 1)
    //key.Down -> string.drop_end(value, 1)
    key.Home -> Focused(..focused, cursor: 0)
    key.End -> Focused(..focused, cursor: string.length(focused.value))
    key.Right -> {
      let cursor = int.min(string.length(focused.value), focused.cursor + 1)
      let offset = input_offset(cursor, focused.offset, focused.width)
      Focused(..focused, cursor:, offset:)
    }
    key.Left -> {
      let cursor = int.max(0, focused.cursor - 1)
      let offset = input_offset(cursor, focused.offset, focused.width)
      Focused(..focused, cursor:, offset:)
    }
    key.Delete -> {
      let offset = case focused.cursor == string.length(focused.value) {
        True -> focused.offset
        False -> int.max(0, focused.offset - 1)
      }
      Focused(
        ..focused,
        value: focused.value |> string_backspace(focused.cursor, 0),
        offset:,
      )
    }
    key.Char(char) -> {
      let cursor = focused.cursor + 1
      let offset = input_offset(cursor, focused.offset, focused.width)
      Focused(
        ..focused,
        value: string_insert(focused.value, focused.cursor, char),
        cursor:,
        offset:,
      )
    }
    _ -> focused
  }
}

fn input_offset(cursor cursor: Int, offset offset: Int, width width: Int) -> Int {
  case cursor {
    x if x < offset -> cursor
    x if x >= offset + { width - 3 } -> cursor - width + 3
    _ -> offset
  }
}

fn string_insert(str: String, cursor: Int, char: String) -> String {
  case cursor {
    x if x <= 0 -> char <> str
    _ -> {
      str
      |> string.to_graphemes
      |> list.index_map(fn(i, idx) {
        case idx == { cursor - 1 } {
          True -> i <> char
          False -> i
        }
      })
      |> string.join("")
    }
  }
}

fn string_backspace(str: String, cursor: Int, offset: Int) -> String {
  str
  |> string.to_graphemes
  |> list.index_fold([], fn(acc, i, idx) {
    case idx == { cursor + offset } {
      True -> acc
      False -> [i, ..acc]
    }
  })
  |> list.reverse
  |> string.join("")
}

//
// CONTROL
//

type Control {
  FocusClear
  FocusNext
  FocusPrev
  ModeInsert
}

fn control_event(input: Key, keybinds: Keybinds) -> Option(Control) {
  case input {
    x if x == keybinds.focus_clear -> FocusClear |> Some
    x if x == keybinds.focus_next -> FocusNext |> Some
    x if x == keybinds.focus_prev -> FocusPrev |> Some
    x if x == keybinds.mode_insert -> ModeInsert |> Some
    _ -> None
  }
}

//
// RENDER
//

// TODO: unpublic
pub type Pos {
  Pos(x: Int, y: Int, width: Int, height: Int)
}

fn render(state: State(model, msg), node: Node(msg), last_input: Key) {
  let pos = Pos(0, 0, state.width, state.height)
  c(BSU) |> io.print
  {
    c(Clear)
    <> node |> render_node(state, _, last_input, pos) |> option.unwrap("")
  }
  |> io.print
  c(ESU) |> io.print
}

fn render_node(
  state: State(model, msg),
  node: Node(msg),
  last_input: Key,
  pos: Pos,
) -> Option(String) {
  case node {
    Layouts(l) -> {
      layout(l, pos.width, pos.height)
      |> list.map(fn(i) {
        let cursor = c(SetPos({ i.1 }.y, { i.1 }.x))
        render_node(state, i.0, last_input, i.1)
        |> option.map(fn(r) { cursor <> r })
      })
      |> option.values
      |> string.join("")
      |> Some
    }
    Split(splits) ->
      case splits {
        Split1(node) -> render_node(state, node, last_input, pos)
        Split2(direction:, ratio:, a:, b:) ->
          case direction {
            Horizontal ->
              [
                c(SetPos(pos.x, pos.y)) |> Some,
                render_node(
                  state,
                  Split(a),
                  last_input,
                  Pos(
                    ..pos,
                    width: pos.width,
                    height: calc_ratio(pos.height, ratio.a, ratio.b),
                  ),
                ),
                c(SetPos(
                  pos.x + 1 + calc_ratio(pos.height, ratio.a, ratio.b),
                  pos.y,
                ))
                  |> Some,
                render_node(
                  state,
                  Split(b),
                  last_input,
                  Pos(
                    x: pos.x + 1 + calc_ratio(pos.height, ratio.a, ratio.b),
                    y: pos.y,
                    width: pos.width,
                    height: calc_ratio(pos.height, ratio.b, ratio.a),
                  ),
                ),
              ]
              |> option.values
              |> string.join("")
              |> Some
            Vertical ->
              [
                c(SetPos(pos.x, pos.y)) |> Some,
                render_node(
                  state,
                  Split(a),
                  last_input,
                  Pos(
                    ..pos,
                    width: calc_ratio(pos.width, ratio.a, ratio.b),
                    height: pos.height,
                  ),
                ),
                c(SetPos(
                  pos.x,
                  pos.y + 1 + calc_ratio(pos.width, ratio.a, ratio.b),
                ))
                  |> Some,
                render_node(
                  state,
                  Split(b),
                  last_input,
                  Pos(
                    x: pos.x,
                    y: pos.y + calc_ratio(pos.width, ratio.a, ratio.b) + 1,
                    width: calc_ratio(pos.width, ratio.b, ratio.a),
                    height: pos.height,
                  ),
                ),
              ]
              |> option.values
              |> string.join("")
              |> Some
          }
      }
    Debug -> string.inspect(pos) |> Some
    Div(children, separator) ->
      list.map(children, render_node(state, _, last_input, pos))
      |> option.values
      |> string.join(sep(separator))
      |> string.append(c(SavePos), _)
      |> Some
    Box(children, title) -> {
      let pos = Pos(..pos, width: pos.width - 3, height: pos.height - 2)
      [
        draw_box(int.max(pos.width, 1), int.max(pos.height, 1), title),
        ..list.map(children, render_node(state, _, last_input, pos))
        |> option.values
      ]
      |> string.join(sep(In))
      |> Some
    }
    Table(width:, table:) ->
      draw_table(int.min(width, pos.width), table) |> Some
    Button(text, input, _) ->
      draw_btn(Btn(10, 1, "", text, last_input == input)) |> Some
    KeyBind(..) -> None
    Input(label, value, width, _event, style) -> {
      let width = calc_size_input(width, pos.width, label)
      let #(is_focused, cursor) = case state.focused {
        Some(focused) if focused.label == label -> #(True, focused.cursor)
        _ -> #(False, string.length(value))
      }
      let is_insert = case state.mode {
        Insert -> True
        _ -> False
      }
      let offset = case state.focused {
        Some(focused) if focused.label == label -> focused.offset
        _ -> input_offset(string.length(value), 0, width)
      }
      draw_input(Iput(
        width,
        1,
        label,
        value,
        is_focused,
        is_insert,
        cursor,
        offset,
        style,
      ))
      |> Some
    }
    Text(text, fg, bg) ->
      {
        c(Reset)
        <> { option.map(fg, fn(o) { c(Fg(o)) }) |> option.unwrap("") }
        <> { option.map(bg, fn(o) { c(Bg(o)) }) |> option.unwrap("") }
        <> text |> string.slice(0, pos.width - 2)
        <> c(Reset)
      }
      |> Some

    TextMulti(text, fg, bg) ->
      {
        c(Reset)
        <> { option.map(fg, fn(o) { c(Fg(o)) }) |> option.unwrap("") }
        <> { option.map(bg, fn(o) { c(Bg(o)) }) |> option.unwrap("") }
        <> text |> text_to_multi(pos.width, pos.height)
        <> c(Reset)
      }
      |> Some

    HR -> string.repeat("─", pos.width) |> Some
    HR2(color) ->
      { c(Fg(color)) <> string.repeat("─", pos.width) <> c(Reset) } |> Some
    Bar(color) ->
      [
        c(SavePos),
        c(Reset),
        c(Bg(color)),
        string.repeat(" ", pos.width),
        c(Reset),
        c(LoadPos),
      ]
      |> string.join("")
      |> Some
    BR -> "\n" |> Some
    Progress(width:, max:, value:, color:) ->
      draw_progress(width:, max:, value:, color:) |> Some
  }
}

fn text_to_multi(text: String, width: Int, height: Int) -> String {
  c(SavePos)
  <> {
    text
    |> string.split("\n")
    |> list.take(height)
    |> list.map(string.slice(_, 0, width - 2))
    |> string.join(c(LoadPos) <> c(Down(1)) <> c(SavePos))
  }
  <> c(LoadPos)
  <> c(Down(1))
}

fn sep(separator: Separator) -> String {
  case separator {
    Row -> c(Right(1))
    Col | In -> c(LoadPos) <> c(Down(1)) <> c(SavePos)
  }
}

fn calc_ratio(of x: Int, for a: Ratio, minus b: Ratio) -> Int {
  case a {
    Px(y) -> y
    Pct(y) -> x * y / 100
    Fill ->
      x
      - case b {
        Px(y) -> y
        Pct(y) -> x * y / 100
        Fill -> x * 50 / 100
      }
  }
}

//
// READ INPUT
//

@external(erlang, "io", "get_chars")
fn get_chars(prompt: String, count: Int) -> String

fn read_input(shore: Subject(Event(msg)), exit: Key) -> Nil {
  let key = get_chars("", 10) |> key.from_string
  case key == exit {
    True -> Exit |> process.send(shore, _)
    False -> key |> KeyPress |> process.send(shore, _)
  }
  read_input(shore, exit)
}

//
// DRAW
//

pub type Node(msg) {
  /// A field for text input
  Input(
    label: String,
    value: String,
    width: Ratio,
    event: fn(String) -> msg,
    style: Style,
  )
  /// A horizontal line
  HR
  HR2(color: Color)
  /// A row with a background color
  Bar(color: Color)
  /// An empty line
  BR
  /// A text string
  Text(text: String, fg: Option(Color), bg: Option(Color))
  /// A multi-line text string
  TextMulti(text: String, fg: Option(Color), bg: Option(Color))
  /// A button assigned to a key press to execute an event
  Button(text: String, key: Key, event: msg)
  /// A non-visible button assigned to a key press to execute an event
  KeyBind(key: Key, event: msg)
  /// A container element for holding other nodes
  Div(children: List(Node(msg)), separator: Separator)
  /// A box container element for holding other nodes
  Box(children: List(Node(msg)), title: Option(String))
  /// A table layout
  Table(width: Int, table: List(List(String)))
  /// TODO: document
  Split(Splits(msg))
  Debug
  // progress bar
  Progress(width: Int, max: Int, value: Int, color: Color)
  // TODO
  Layouts(layout: Layout(msg))
}

/// TODO
pub type Splits(msg) {
  Split1(node: Node(msg))
  Split2(direction: Direction, ratio: Ratio2, a: Splits(msg), b: Splits(msg))
}

pub type Ratio2 {
  Ratio2(a: Ratio, b: Ratio)
}

// TODO: rename size and remove old size
pub type Ratio {
  Px(Int)
  Pct(Int)
  Fill
}

fn calc_size(size: Ratio, width: Int) -> Int {
  case size {
    Px(px) -> px
    Pct(pct) -> width * pct / 100
    Fill -> width
  }
}

fn calc_size_input(size: Ratio, width: Int, label: String) -> Int {
  calc_size(size, width - string.length(label))
}

pub type Direction {
  Horizontal
  Vertical
}

pub type Separator {
  Row
  Col
  In
}

pub type Size {
  Fixed(Int)
}

pub type Style {
  Simple
  Border
}

fn do_middle(width: Int, height: Int, acc: List(String)) -> String {
  case height {
    0 -> acc |> string.join(c(Left(width + 2)) <> c(Down(1)))
    h -> do_middle(width, h - 1, [middle(width), ..acc])
  }
}

fn middle(width: Int) -> String {
  let fill = " "
  ["│", string.repeat(fill, width), "│"] |> string.join("")
}

type Btn {
  Btn(width: Int, height: Int, title: String, text: String, pressed: Bool)
}

type Iput {
  Iput(
    width: Int,
    height: Int,
    title: String,
    text: String,
    pressed: Bool,
    insert: Bool,
    cursor: Int,
    offset: Int,
    style: Style,
  )
}

fn draw_input(btn: Iput) -> String {
  // 2 == padding of space either side
  let color = case btn.pressed, btn.insert {
    True, True -> Green |> Fg |> c
    True, False -> Blue |> Fg |> c
    _, _ -> White |> Fg |> c
  }
  let in_width = btn.width - 2
  let text = btn.text <> " "
  let padding = in_width - string.length(text)
  let text_trim =
    text
    |> string.slice(btn.offset, in_width)
    |> fn(x) {
      case btn.pressed {
        True -> map_cursor(x, btn.cursor - btn.offset, btn.width)
        False -> x
      }
    }
  let top = case string.length(btn.title) {
    0 -> fn() { ["╭", string.repeat("─", btn.width), "╮"] |> string.join("") }
    _ -> fn() {
      [
        "╭",
        "─",
        " ",
        btn.title,
        " ",
        string.repeat("─", btn.width - 3 - string.length(btn.title)),
        "╮",
      ]
      |> string.join("")
    }
  }
  let middle = fn() {
    [
      "│",
      " ",
      c(Reset),
      text_trim,
      string.repeat(" ", padding),
      color,
      " ",
      "│",
    ]
    |> string.join("")
  }
  let bottom = fn() {
    ["╰", string.repeat("─", btn.width), "╯"] |> string.join("")
  }
  let width = btn.width + 2
  let start = c(Left(width)) <> c(Down(1))
  let top_right = c(Up(1 + btn.height))

  case btn.style {
    Simple -> [color, btn.title, " ", c(Reset), text_trim] |> string.join("")
    Border ->
      [color, top(), start, middle(), start, bottom(), top_right, Reset |> c]
      |> string.join("")
  }
}

fn map_cursor(str: String, cursor: Int, width: Int) -> String {
  let pos = cursor |> int.min(width - 2)
  str
  |> string.to_graphemes
  |> list.index_map(fn(i, idx) {
    case idx == pos {
      True -> c(Bg(White)) <> c(Fg(Black)) <> i <> c(Reset)
      False -> i
    }
  })
  |> string.join("")
}

fn draw_btn(btn: Btn) -> String {
  let button = "  " <> btn.text <> "  "
  let bg = case btn.pressed {
    False -> Blue |> Bg |> c
    True -> Green |> Bg |> c
  }
  [bg, c(Fg(Black)), button, c(Reset)]
  |> string.join("")
}

fn draw_box(width: Int, height: Int, title: Option(String)) -> String {
  let top =
    case title {
      Some(title) -> [
        "╭",
        "─",
        " ",
        title,
        " ",
        string.repeat("─", width - 3 - string.length(title)),
        "╮",
      ]
      None -> ["╭", string.repeat("─", width), "╮"]
    }
    |> string.join("")
  let bottom = ["╰", string.repeat("─", width), "╯"] |> string.join("")
  let start = c(Left(width + 2)) <> c(Down(1))
  [
    c(SavePos),
    top,
    start,
    do_middle(width, height, []),
    start,
    bottom,
    c(LoadPos),
    c(Right(2)),
    c(SavePos),
  ]
  |> string.join("")
}

fn draw_progress(
  width width: Int,
  max max: Int,
  value value: Int,
  color color: Color,
) -> String {
  let progress = value * 100 / max
  let complete = progress * width / 100 |> int.min(width)
  let rest = width - complete
  [
    c(Reset),
    c(Fg(color)),
    string.repeat("█", complete),
    c(Reset),
    string.repeat("░", rest),
  ]
  |> string.join("")
}

type TableAttr {
  TableAttr(
    width: Int,
    col_count: Int,
    row_count: Int,
    col_width: Int,
    row_height: Int,
  )
}

fn draw_table(width: Int, values: List(List(String))) -> String {
  let col_count =
    values |> list.first |> result.map(list.length) |> result.unwrap(1)
  let col_width = { width - 2 } / col_count
  let col_left_over = width - col_width * col_count
  let row_count = values |> list.length
  let row_height = 1
  let table = TableAttr(width:, col_count:, col_width:, row_count:, row_height:)

  let top = ["╭", string.repeat("─", table.width), "╮"] |> string.join("")
  let start = c(Left(width + 2)) <> c(Down(1))
  let row = fn(row, idx) {
    list.map(row, fn(col) {
      case string.length(col) {
        x if x >= col_width -> string.slice(col, 0, col_width - 3) <> ".. "
        x -> col <> c(Right(col_width - x))
      }
    })
    |> string.join("")
    |> fn(row) {
      case idx {
        0 ->
          "│"
          <> c(SGR(Bold))
          <> c(Fg(Blue))
          <> row
          <> c(Reset)
          <> c(Right(col_left_over))
          <> "│"
          <> start
        _ -> "│" <> row <> c(Right(col_left_over)) <> "│" <> start
      }
    }
  }
  let rows = values |> list.index_map(row) |> string.join("")
  let bottom = ["╰", string.repeat("─", table.width), "╯"] |> string.join("")
  [c(Reset), top, start, rows, bottom, start]
  |> string.join("")
}

//
// TERMINAL CODES
//

const esc = "\u{001b}"

type TermCode {
  Clear
  Top
  HideCursor
  ShowCursor
  SetPos(x: Int, y: Int)
  SavePos
  LoadPos
  Up(Int)
  Down(Int)
  Left(Int)
  Right(Int)
  StartLine
  Column(Int)
  Fg(Color)
  Bg(Color)
  SGR(Graphic)
  Reset
  GetPos
  AltBuffer
  MainBuffer
  /// Begin Synchronized Output
  BSU
  /// End Synchronized Output
  ESU
}

fn c(code: TermCode) -> String {
  case code {
    Clear -> esc <> "[2J" <> esc <> "[H"
    Top -> esc <> "[H"
    HideCursor -> esc <> "[?25l"
    ShowCursor -> esc <> "[?25h"
    SetPos(x, y) ->
      esc <> "[" <> int.to_string(x) <> ";" <> int.to_string(y) <> "H"
    SavePos -> esc <> "[s"
    LoadPos -> esc <> "[u"
    Up(i) -> esc <> "[" <> int.to_string(i) <> "A"
    Down(i) -> esc <> "[" <> int.to_string(i) <> "B"
    Left(i) -> esc <> "[" <> int.to_string(i) <> "D"
    Right(i) -> esc <> "[" <> int.to_string(i) <> "C"
    StartLine -> Column(1) |> c
    Column(i) -> esc <> "[" <> int.to_string(i) <> "G"
    Fg(color) -> esc <> "[3" <> col(color) <> "m"
    Bg(color) -> esc <> "[4" <> col(color) <> "m"
    SGR(graphic) -> esc <> "[" <> graphic_to_string(graphic) <> "m"
    Reset -> esc <> "[0m"
    GetPos -> esc <> "[6n"
    AltBuffer -> esc <> "[?1049h"
    MainBuffer -> esc <> "[?1049l"
    BSU -> esc <> "[?2026h"
    ESU -> esc <> "[?2026l"
  }
}

//
// GRAPHIC
//

pub type Graphic {
  Bold
  Faint
  Italic
  Underline
}

fn graphic_to_string(graphic: Graphic) -> String {
  case graphic {
    Bold -> "1"
    Faint -> "2"
    Italic -> "3"
    Underline -> "4"
  }
}

//
// COLOR
//

pub type Color {
  Black
  Red
  Green
  Yellow
  Blue
  Magenta
  Cyan
  White
}

fn col(color: Color) -> String {
  case color {
    Black -> "0"
    Red -> "1"
    Green -> "2"
    Yellow -> "3"
    Blue -> "4"
    Magenta -> "5"
    Cyan -> "6"
    White -> "7"
  }
}

//
// SHELL
//

type ShellOpt {
  Noshell
  Raw
}

fn raw_erl() {
  raw_ffi(#(Noshell, Raw))
}

type DoNotLeak

@external(erlang, "shell", "start_interactive")
fn raw_ffi(opts: #(ShellOpt, ShellOpt)) -> DoNotLeak

type TODO

@external(erlang, "io", "rows")
fn terminal_rows() -> Result(Int, TODO)

@external(erlang, "io", "columns")
fn terminal_columns() -> Result(Int, TODO)
