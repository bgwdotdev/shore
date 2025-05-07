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

pub fn main() {
  io.print(c(MoveRight(0)) <> "a")
}

//
// LAYOUT
//

pub type Layout(msg) {
  Grid(gap: Int, rows: List(Size), columns: List(Size), cells: List(Cell(msg)))
}

pub type Cell(msg) {
  Cell(content: Node(msg), row: #(Int, Int), col: #(Int, Int))
}

pub fn layout(
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
    #(cell.content, Pos(x:, y:, width: w, height: h, align: Left))
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

fn calc_sizes(max: Int, sizes: List(Size)) {
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

pub fn layout_center(
  content: Node(msg),
  width: Size,
  height: Size,
) -> Layout(msg) {
  Grid(gap: 0, rows: [Fill, height, Fill], columns: [Fill, width, Fill], cells: [
    Cell(content:, row: #(1, 1), col: #(1, 1)),
  ])
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
    Button(key:, event:, ..) if input == key -> Some(event)
    Button(..) -> None
    KeyBind(key, event) if input == key -> Some(event)
    KeyBind(..) -> None
    Aligned(node:, ..) -> detect_event(state, node, input)
    DivRow(children:) | DivCol(children:) | Box(children:, ..) ->
      do_detect_event(state, children, input)
    Layouts(layout) ->
      layout.cells
      |> list.map(fn(cell) { detect_event(state, cell.content, input) })
      |> option.values
      |> list.first
      |> option.from_result
    Input(..)
    | HR
    | HR2(..)
    | Bar(..)
    | BR
    | Progress(..)
    | Table(..)
    | Text(..)
    | TextMulti(..)
    | Debug -> None
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
  let pos = Pos(0, 0, state.width, state.height, Left)
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
        DivRow(children) | DivCol(children) ->
          do_list_focusable(pos, xs, do_list_focusable(pos, children, acc))
        Box(children:, ..) -> {
          let pos = Pos(..pos, width: pos.width - 4, height: pos.height - 2)
          do_list_focusable(pos, xs, do_list_focusable(pos, children, acc))
        }
        Layouts(l) -> {
          layout(l, pos.width, pos.height)
          |> list.map(fn(i) { do_list_focusable(i.1, [i.0], acc) })
          |> list.flatten
        }
        Input(label, value, width, event, ..) -> {
          let cursor = string.length(value)
          let width = calc_size_input(width, pos.width, label)
          let focused =
            Focused(label:, value:, event:, offset: 0, cursor:, width:)
          let offset = input_offset(cursor, focused.offset, focused.width)
          do_list_focusable(pos, xs, [Focused(..focused, offset:), ..acc])
        }
        Aligned(..)
        | BR
        | Bar(..)
        | Button(..)
        | Debug
        | HR
        | HR2(..)
        | KeyBind(..)
        | Progress(..)
        | Table(..)
        | Text(..)
        | TextMulti(..) -> do_list_focusable(pos, xs, acc)
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
// ELEMENT
//

type Element {
  Element(content: String, width: Int, height: Int)
}

fn element_join(elements: List(Element), separator: String) -> Element {
  case elements {
    [] -> Element("", 0, 0)
    [first, ..rest] -> element_join_loop(rest, separator, first)
  }
}

fn element_join_loop(
  elements: List(Element),
  separator: String,
  accumulator: Element,
) -> Element {
  case elements {
    [] -> accumulator
    [element, ..elements] ->
      element_join_loop(
        elements,
        separator,
        Element(
          content: accumulator.content <> separator <> element.content,
          width: accumulator.width + element.width,
          height: accumulator.height + element.height,
        ),
      )
  }
}

fn element_append(to first: Element, suffix second: Element) -> Element {
  Element(
    content: first.content <> second.content,
    width: first.width + second.width,
    height: first.height + second.height,
  )
}

fn element_prefix(element: Element, prefix: String) -> Element {
  Element(..element, content: prefix <> element.content)
}

//
// RENDER
//

// TODO: unpublic
pub type Pos {
  Pos(x: Int, y: Int, width: Int, height: Int, align: Align)
}

fn render(state: State(model, msg), node: Node(msg), last_input: Key) {
  let pos = Pos(0, 0, state.width, state.height, Left)
  c(BSU) |> io.print
  {
    c(Clear)
    <> node
    |> render_node(state, _, last_input, pos)
    |> option.map(fn(r) { r.content })
    |> option.unwrap("")
  }
  |> io.print
  c(ESU) |> io.print
}

fn render_node(
  state: State(model, msg),
  node: Node(msg),
  last_input: Key,
  pos: Pos,
) -> Option(Element) {
  case node {
    Layouts(l) -> {
      layout(l, pos.width, pos.height)
      |> list.map(fn(i) {
        let cursor = c(SetPos({ i.1 }.y, { i.1 }.x))
        render_node(state, i.0, last_input, i.1)
        |> option.map(fn(r) { element_prefix(r, cursor) })
      })
      |> option.values
      |> element_join("")
      |> Some
    }
    Debug -> {
      let content = string.inspect(pos)
      let width = string.length(content)
      Element(content:, width:, height: 1) |> Some
    }
    Aligned(align, node) ->
      render_node(state, node, last_input, Pos(..pos, align:))
    DivRow(children) -> {
      let len = children |> list.length
      let width = pos.width / len
      list.index_map(children, fn(child, idx) {
        let x = idx * width
        let new_pos = Pos(..pos, x:, width:, align: right_is_left(pos.align))
        render_node(state, child, last_input, new_pos)
        |> option.map(fn(r) {
          let move = case pos.align {
            Center -> c(MoveRight(x))
            Left | Right -> ""
          }
          element_prefix(r, move)
        })
      })
      |> option.values
      |> element_join(sep(Row))
      |> fn(ele) {
        case pos.align {
          Right -> c(MoveRight(pos.width - ele.width - 1))
          Left | Center -> ""
        }
        |> element_prefix(ele, _)
      }
      |> Some
    }
    DivCol(children) -> {
      list.map(children, render_node(state, _, last_input, pos))
      |> option.values
      |> element_join(sep(Col))
      |> element_prefix(c(SavePos))
      |> Some
    }
    Box(children, title) -> {
      // TODO: review how box grid gaps implement
      let pos = Pos(..pos, width: pos.width - 3, height: pos.height - 2)
      let pos_child = Pos(..pos, width: pos.width - 2)
      [
        draw_box(int.max(pos.width, 1), int.max(pos.height, 1), title),
        ..list.map(children, render_node(state, _, last_input, pos_child))
        |> option.values
      ]
      |> element_join(sep(In))
      |> Some
    }
    Table(width:, table:) ->
      draw_table(int.min(width, pos.width), table) |> Some
    Button(text, input, _) ->
      draw_btn(Btn(pos.width, 1, "", text, last_input == input, pos.align))
      |> Some
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
    Text(text, fg, bg) -> draw_text(text, fg, bg, pos) |> Some
    TextMulti(text, fg, bg) -> draw_text_multi(text, fg, bg, pos) |> Some

    HR ->
      string.repeat("─", pos.width)
      |> Element(width: pos.width, height: 1)
      |> Some
    HR2(color) ->
      { c(Fg(color)) <> string.repeat("─", pos.width) <> c(Reset) }
      |> Element(width: pos.width, height: 1)
      |> Some
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
      |> Element(width: pos.width, height: 1)
      |> Some
    BR -> "\n" |> Element(width: pos.width, height: 1) |> Some
    Progress(width:, max:, value:, color:) -> {
      let width = calc_size(width, pos.width)
      draw_progress(width:, max:, value:, color:, pos:) |> Some
    }
  }
}

fn sep(separator: Separator) -> String {
  case separator {
    Row -> c(MoveRight(1))
    Col | In -> c(LoadPos) <> c(MoveDown(1)) <> c(SavePos)
  }
}

//
// DRAW
//

pub type Node(msg) {
  /// A field for text input
  Input(
    label: String,
    value: String,
    width: Size,
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
  /// Sets alignment of all child nodes
  Aligned(align: Align, node: Node(msg))
  /// A container element for holding other nodes over multiple lines
  DivCol(children: List(Node(msg)))
  /// A container element for holding other nodes in a single line
  DivRow(children: List(Node(msg)))
  /// A box container element for holding other nodes
  Box(children: List(Node(msg)), title: Option(String))
  /// A table layout
  Table(width: Int, table: List(List(String)))
  Debug
  // progress bar
  Progress(width: Size, max: Int, value: Int, color: Color)
  // TODO
  Layouts(layout: Layout(msg))
}

pub type Size {
  Px(Int)
  Pct(Int)
  Fill
}

fn calc_size(size: Size, width: Int) -> Int {
  case size {
    Px(px) -> px
    Pct(pct) -> width * pct / 100
    Fill -> width
  }
}

fn calc_size_input(size: Size, width: Int, label: String) -> Int {
  calc_size(size, width - string.length(label))
}

pub type Separator {
  Row
  Col
  In
}

pub type Style {
  Simple
  Border
}

pub type Align {
  Left
  Center
  Right
}

fn calc_align(text: String, align: Align, width: Int) -> String {
  let len = text |> string.length
  calc_align_len(text, align, width, len)
}

fn calc_align_len(text: String, align: Align, width: Int, len: Int) -> String {
  let center = { width / 2 } - { len / 2 }
  case align {
    Left -> text
    Center -> c(SavePos) <> c(MoveRight(center)) <> text <> c(LoadPos)
    Right -> c(SavePos) <> c(MoveRight(width - len)) <> text <> c(LoadPos)
  }
}

fn right_is_left(align: Align) -> Align {
  case align {
    Right -> Left
    x -> x
  }
}

fn do_middle(width: Int, height: Int, acc: List(String)) -> String {
  case height {
    0 -> acc |> string.join(c(MoveLeft(width + 2)) <> c(MoveDown(1)))
    h -> do_middle(width, h - 1, [middle(width), ..acc])
  }
}

fn middle(width: Int) -> String {
  let fill = " "
  ["│", string.repeat(fill, width), "│"] |> string.join("")
}

fn draw_text(
  text: String,
  fg: Option(Color),
  bg: Option(Color),
  pos: Pos,
) -> Element {
  let width = pos.width - 2
  text
  |> string.slice(0, width)
  |> calc_align(pos.align, pos.width)
  |> style_text(fg, bg)
  |> Element(width:, height: 1)
}

fn style_text(text: String, fg: Option(Color), bg: Option(Color)) -> String {
  c(Reset)
  <> { option.map(fg, fn(o) { c(Fg(o)) }) |> option.unwrap("") }
  <> { option.map(bg, fn(o) { c(Bg(o)) }) |> option.unwrap("") }
  <> text
  <> c(Reset)
}

fn draw_text_multi(
  text: String,
  fg: Option(Color),
  bg: Option(Color),
  pos: Pos,
) -> Element {
  let width = pos.width - 2
  let height = pos.height
  let text =
    c(SavePos)
    <> {
      text
      |> string.split("\n")
      |> list.take(height)
      |> list.map(string.slice(_, 0, width))
      |> list.map(calc_align(_, pos.align, pos.width))
      |> string.join(c(LoadPos) <> c(MoveDown(1)) <> c(SavePos))
    }
    <> c(LoadPos)
    <> c(MoveDown(1))
  text
  |> style_text(fg, bg)
  |> Element(width:, height:)
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

fn draw_input(btn: Iput) -> Element {
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
  let start = c(MoveLeft(width)) <> c(MoveDown(1))
  let top_right = c(MoveUp(1 + btn.height))

  case btn.style {
    Simple -> [color, btn.title, " ", c(Reset), text_trim] |> string.join("")
    Border ->
      [color, top(), start, middle(), start, bottom(), top_right, Reset |> c]
      |> string.join("")
  }
  |> Element(width: btn.width, height: btn.height)
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

type Btn {
  Btn(
    width: Int,
    height: Int,
    title: String,
    text: String,
    pressed: Bool,
    align: Align,
  )
}

fn draw_btn(btn: Btn) -> Element {
  let button = {
    "  " <> btn.text <> "  "
  }
  let width = string.length(button)
  let button = button |> calc_align(btn.align, btn.width)
  let bg = case btn.pressed {
    False -> Blue |> Bg |> c
    True -> Green |> Bg |> c
  }
  [bg, c(Fg(Black)), button, c(Reset)]
  |> string.join("")
  |> Element(width:, height: 1)
}

fn draw_box(width: Int, height: Int, title: Option(String)) -> Element {
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
  let start = c(MoveLeft(width + 2)) <> c(MoveDown(1))
  [
    c(SavePos),
    top,
    start,
    do_middle(width, height, []),
    start,
    bottom,
    c(LoadPos),
    c(MoveRight(2)),
    c(SavePos),
  ]
  |> string.join("")
  |> Element(width:, height:)
}

fn draw_progress(
  width width: Int,
  max max: Int,
  value value: Int,
  color color: Color,
  pos pos: Pos,
) -> Element {
  let progress = value * 100 / max
  let complete = progress * width / 100 |> int.min(width)
  let rest = width - complete
  let len = complete + rest
  [
    c(Reset),
    c(Fg(color)),
    string.repeat("█", complete),
    c(Reset),
    string.repeat("░", rest),
  ]
  |> string.join("")
  |> calc_align_len(pos.align, pos.width, len)
  |> Element(width:, height: 1)
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

fn draw_table(width: Int, values: List(List(String))) -> Element {
  let col_count =
    values |> list.first |> result.map(list.length) |> result.unwrap(1)
  let col_width = { width - 2 } / col_count
  let col_left_over = width - col_width * col_count
  let row_count = values |> list.length
  let row_height = 1
  let table = TableAttr(width:, col_count:, col_width:, row_count:, row_height:)

  let top = ["╭", string.repeat("─", table.width), "╮"] |> string.join("")
  let start = c(MoveLeft(width + 2)) <> c(MoveDown(1))
  let row = fn(row, idx) {
    list.map(row, fn(col) {
      case string.length(col) {
        x if x >= col_width -> string.slice(col, 0, col_width - 3) <> ".. "
        x -> col <> c(MoveRight(col_width - x))
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
          <> c(MoveRight(col_left_over))
          <> "│"
          <> start
        _ -> "│" <> row <> c(MoveRight(col_left_over)) <> "│" <> start
      }
    }
  }
  let rows = values |> list.index_map(row) |> string.join("")
  let bottom = ["╰", string.repeat("─", table.width), "╯"] |> string.join("")
  [c(Reset), top, start, rows, bottom, start]
  |> string.join("")
  |> Element(width:, height: row_count + 2)
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
  MoveUp(Int)
  MoveDown(Int)
  MoveLeft(Int)
  MoveRight(Int)
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
    MoveUp(i) -> esc <> "[" <> int.to_string(i) <> "A" |> ignore_zero(i)
    MoveDown(i) -> esc <> "[" <> int.to_string(i) <> "B" |> ignore_zero(i)
    MoveLeft(i) -> esc <> "[" <> int.to_string(i) <> "D" |> ignore_zero(i)
    MoveRight(i) -> esc <> "[" <> int.to_string(i) <> "C" |> ignore_zero(i)
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

// zero may act like one for move commands for certain terminal emulators
fn ignore_zero(code: String, i: Int) -> String {
  case i {
    0 -> ""
    _ -> code
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
