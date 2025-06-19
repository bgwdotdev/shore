import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import shore/key.{type Key}
import shore/style

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
    last_input: String,
    focused: Option(Focused(msg)),
  )
}

pub type Keybinds {
  Keybinds(
    exit: Key,
    submit: Key,
    focus_clear: Key,
    focus_next: Key,
    focus_prev: Key,
  )
}

type Focused(msg) {
  FocusedInput(
    label: String,
    value: String,
    event: fn(String) -> msg,
    cursor: Int,
    offset: Int,
    width: Int,
  )
  FocusedButton(label: String, event: msg)
}

pub fn start(
  spec: Spec(model, msg),
) -> Result(Subject(Event(msg)), actor.StartError) {
  raw_erl()
  { c(HideCursor) <> c(AltBuffer) } |> io.print
  use actor.Started(data: shore, ..) <- result.map(shore_start(spec))
  process.spawn(fn() { read_input(shore, spec.keybinds.exit) })
  redraw_on_timer(spec, shore)
  shore
}

fn shore_start(spec: Spec(model, msg)) {
  actor.new_with_initialiser(1000, fn(tasks) {
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
        last_input: "",
        focused: None,
      )
    let _first_paint = model |> spec.view |> render(state, _, key.Null)
    task_init |> task_handler(tasks)
    Ok(actor.returning(actor.initialised(state), tasks))
  })
  |> actor.on_message(shore_loop)
  |> actor.start
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

/// Events can be sent to the subject of the shore actor. Use the `cmd` and `exit` functions.
pub opaque type Event(msg) {
  KeyPress(Key)
  Cmd(msg)
  Redraw
  Exit
}

/// Allows sending a message to your TUI from another actor. This can be used,
/// for example, to push an event to your TUI, rather than have it poll.
///
pub fn send(msg: msg) -> Event(msg) {
  Cmd(msg)
}

/// Manually trigger the exit for your TUI. Normally this would be handled
/// through the exit keybind.
///
pub fn exit() -> Event(msg) {
  Exit
}

fn shore_loop(state: State(model, msg), event: Event(msg)) {
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
      let ui = state.spec.view(state.model)
      let state = case state.focused {
        None -> {
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
          state
        }
        Some(focused) -> {
          let reload = list_focusable([ui], state) |> focus_current(focused)
          case reload {
            Some(focused) -> {
              case input_handler(focused, input) {
                FocusedInput(..) as focused -> {
                  let #(model, tasks) =
                    state.spec.update(state.model, focused.event(focused.value))
                  tasks |> task_handler(state.tasks)
                  let state = State(..state, focused: Some(focused), model:)
                  let state = update_viewport(state)
                  redraw_on_update(state, input)
                  state
                }
                FocusedButton(..) as focused -> {
                  case input == state.spec.keybinds.submit {
                    True -> {
                      let #(model, tasks) =
                        state.spec.update(state.model, focused.event)
                      tasks |> task_handler(state.tasks)
                      let state = State(..state, focused: Some(focused), model:)
                      let state = update_viewport(state)
                      redraw_on_update(state, input)
                      state
                    }
                    False -> state
                  }
                }
              }
            }
            // Element has disappeared from screen for reasons
            None -> {
              let state = State(..state, focused: None)
              let state = update_viewport(state)
              redraw_on_update(state, input)
              state
            }
          }
        }
      }
      let state = case control_event(input, state.spec.keybinds) {
        Some(FocusClear) -> State(..state, focused: None)
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
        None -> state
      }
      actor.continue(state)
    }
    Redraw -> {
      let state = update_viewport(state)
      redraw(state, key.Null)
      actor.continue(state)
    }
    Exit -> {
      { c(ShowCursor) <> c(MainBuffer) } |> io.print
      process.send(state.spec.exit, Nil)
      actor.stop()
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
    Aligned(node:, ..) | Bar2(node:, ..) -> detect_event(state, node, input)
    Row(children:) | Col(children:) | Box(children:, ..) ->
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
    | TableKV(..)
    | Graph(..)
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
  OnTimer(ms: Int)
}

fn redraw_on_timer(spec: Spec(model, msg), shore: Subject(Event(msg))) {
  case spec.redraw {
    OnUpdate -> Nil
    OnTimer(x) -> {
      process.spawn(fn() { do_redraw_on_timer(shore, x) })
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
    |> process.spawn_unlinked()
  })
}

//
// FIELD FOCUS
//

fn list_focusable(
  children: List(Node(msg)),
  state: State(model, msg),
) -> List(Focused(msg)) {
  let pos = Pos(0, 0, state.width, state.height, style.Left)
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
        Row(children) | Col(children) ->
          do_list_focusable(pos, xs, do_list_focusable(pos, children, acc))
        Box(children:, ..) -> {
          let pos = Pos(..pos, width: pos.width - 4, height: pos.height - 2)
          do_list_focusable(pos, xs, do_list_focusable(pos, children, acc))
        }
        Layouts(l) -> {
          layout(l, pos)
          |> list.map(fn(i) { do_list_focusable(i.1, [i.0], acc) })
          |> list.flatten
        }
        Input(label, value, width, event, _) -> {
          let cursor = string.length(value)
          let width = calc_size_input(width, pos.width, label)
          let focused =
            FocusedInput(label:, value:, event:, offset: 0, cursor:, width:)
          let offset = input_offset(cursor, focused.offset, focused.width)
          do_list_focusable(pos, xs, [FocusedInput(..focused, offset:), ..acc])
        }
        Button(text:, event:, ..) -> {
          do_list_focusable(pos, xs, [FocusedButton(text, event), ..acc])
        }
        Aligned(..)
        | BR
        | Bar(..)
        | Bar2(..)
        | Debug
        | HR
        | HR2(..)
        | KeyBind(..)
        | Progress(..)
        | Table(..)
        | TableKV(..)
        | Graph(..)
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

fn focus_current(
  focusable: List(Focused(msg)),
  focused: Focused(msg),
) -> Option(Focused(msg)) {
  case focusable {
    [] -> None
    [x, ..xs] ->
      case x.label == focused.label {
        True -> Some(x)
        False -> focus_current(xs, focused)
      }
  }
}

//
// TEXT INPUT
//

fn input_handler(focused: Focused(msg), key: Key) -> Focused(msg) {
  case focused {
    FocusedInput(..) as focused -> {
      case key {
        key.Backspace -> {
          let cursor = int.max(0, focused.cursor - 1)
          let offset = int.max(0, focused.offset - 1)
          FocusedInput(
            ..focused,
            value: focused.value |> string_backspace(focused.cursor, -1),
            cursor:,
            offset:,
          )
        }
        //key.Up -> string.drop_end(value, 1)
        //key.Down -> string.drop_end(value, 1)
        key.Home -> FocusedInput(..focused, cursor: 0)
        key.End -> FocusedInput(..focused, cursor: string.length(focused.value))
        key.Right -> {
          let cursor = int.min(string.length(focused.value), focused.cursor + 1)
          let offset = input_offset(cursor, focused.offset, focused.width)
          FocusedInput(..focused, cursor:, offset:)
        }
        key.Left -> {
          let cursor = int.max(0, focused.cursor - 1)
          let offset = input_offset(cursor, focused.offset, focused.width)
          FocusedInput(..focused, cursor:, offset:)
        }
        key.Delete -> {
          let offset = case focused.cursor == string.length(focused.value) {
            True -> focused.offset
            False -> int.max(0, focused.offset - 1)
          }
          FocusedInput(
            ..focused,
            value: focused.value |> string_backspace(focused.cursor, 0),
            offset:,
          )
        }
        key.Char(char) -> {
          let cursor = focused.cursor + 1
          let offset = input_offset(cursor, focused.offset, focused.width)
          FocusedInput(
            ..focused,
            value: string_insert(focused.value, focused.cursor, char),
            cursor:,
            offset:,
          )
        }
        _ -> focused
      }
    }
    FocusedButton(..) as button -> button
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
}

fn control_event(input: Key, keybinds: Keybinds) -> Option(Control) {
  case input {
    x if x == keybinds.focus_clear -> FocusClear |> Some
    x if x == keybinds.focus_next -> FocusNext |> Some
    x if x == keybinds.focus_prev -> FocusPrev |> Some
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
type Pos {
  Pos(x: Int, y: Int, width: Int, height: Int, align: style.Align)
}

fn render(state: State(model, msg), node: Node(msg), last_input: Key) {
  let pos = Pos(0, 0, state.width, state.height, style.Left)
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
      layout(l, pos)
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
    Row(children) -> {
      let len = children |> list.length
      let width = pos.width / len
      list.index_map(children, fn(child, idx) {
        let x = idx * width
        let new_pos = Pos(..pos, x:, width:, align: right_is_left(pos.align))
        render_node(state, child, last_input, new_pos)
        |> option.map(fn(r) {
          let move = case pos.align {
            style.Center -> c(MoveRight(x))
            style.Left | style.Right -> ""
          }
          element_prefix(r, move)
        })
      })
      |> option.values
      |> element_join(sep(SepRow))
      |> fn(ele) {
        case pos.align {
          style.Right -> c(MoveRight(pos.width - ele.width - { len - 1 }))
          style.Left | style.Center -> ""
        }
        |> element_prefix(ele, _)
      }
      |> Some
    }
    Col(children) -> {
      list.map(children, render_node(state, _, last_input, pos))
      |> option.values
      |> element_join(sep(SepCol))
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
      |> element_join(sep(SepCol))
      |> Some
    }
    Table(width:, table:) -> {
      let width = calc_size(width, pos.width)
      draw_table(int.min(width, pos.width), table, pos) |> Some
    }
    TableKV(width:, table:) -> {
      let width = calc_size(width, pos.width)
      draw_table_kv(int.min(width, pos.width), table, pos) |> Some
    }
    Graph(width:, height:, points:) ->
      draw_graph(
        calc_size(width, pos.width),
        calc_size(height, pos.height),
        points,
      )
      |> Some
    Button(text, input, _) -> {
      let is_focused = case state.focused {
        Some(FocusedButton(..) as focused) if focused.label == text -> True
        _ -> False
      }
      draw_btn(Btn(
        pos.width,
        1,
        "",
        text,
        last_input == input || is_focused,
        pos.align,
      ))
      |> Some
    }
    KeyBind(..) -> None
    Input(label, value, width, _event, hidden) -> {
      let width = calc_size_input(width, pos.width, label)
      let #(is_focused, cursor) = case state.focused {
        Some(FocusedInput(..) as focused) if focused.label == label -> #(
          True,
          focused.cursor,
        )
        _ -> #(False, string.length(value))
      }
      let offset = case state.focused {
        Some(FocusedInput(..) as focused) if focused.label == label ->
          focused.offset
        _ -> input_offset(string.length(value), 0, width)
      }
      draw_input(Iput(
        width,
        1,
        label,
        value,
        is_focused,
        cursor,
        offset,
        hidden,
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
    Bar2(color, node) -> {
      let bar =
        [
          c(SavePos),
          c(Reset),
          c(Bg(color)),
          string.repeat(" ", pos.width),
          c(Reset),
          c(LoadPos),
        ]
        |> string.join("")
      render_node(state, node, last_input, pos)
      |> option.map(element_prefix(_, bar))
    }
    BR -> "\n" |> Element(width: pos.width, height: 1) |> Some
    Progress(width:, max:, value:, color:) -> {
      let width = calc_size(width, pos.width)
      draw_progress(width:, max:, value:, color:, pos:) |> Some
    }
  }
}

fn sep(separator: Separator) -> String {
  case separator {
    SepRow -> c(MoveRight(1))
    SepCol -> c(LoadPos) <> c(MoveDown(1)) <> c(SavePos)
  }
}

//
// DRAW
//

/// All UI elements
pub type Node(msg) {
  /// A field for text input
  Input(
    label: String,
    value: String,
    width: style.Size,
    event: fn(String) -> msg,
    hidden: Bool,
  )
  /// A horizontal line
  HR
  /// A colored horizontal line
  HR2(color: style.Color)
  /// A row with a background color
  Bar(color: style.Color)
  /// A row with a background color, containing items
  Bar2(color: style.Color, node: Node(msg))
  /// An empty line
  BR
  /// A text string
  Text(text: String, fg: Option(style.Color), bg: Option(style.Color))
  /// A multi-line text string
  TextMulti(text: String, fg: Option(style.Color), bg: Option(style.Color))
  /// A button assigned to a key press to execute an event
  Button(text: String, key: Key, event: msg)
  /// A non-visible button assigned to a key press to execute an event
  KeyBind(key: Key, event: msg)
  /// Sets alignment of all child nodes
  Aligned(align: style.Align, node: Node(msg))
  /// A container element for holding other nodes over multiple lines
  Col(children: List(Node(msg)))
  /// A container element for holding other nodes in a single line
  Row(children: List(Node(msg)))
  /// A box container element for holding other nodes
  Box(children: List(Node(msg)), title: Option(String))
  /// A table layout
  Table(width: style.Size, table: List(List(String)))
  /// A Key-Value style table layout
  TableKV(width: style.Size, table: List(List(String)))
  /// An extremely simple plot
  Graph(width: style.Size, height: style.Size, points: List(Float))
  /// Prints some positional information for developer debugging
  Debug
  /// A progress bar, will automatically calculate fill percent based off max and current values
  Progress(width: style.Size, max: Int, value: Int, color: style.Color)
  /// Wraps a `Layout`
  Layouts(layout: Layout(msg))
}

fn calc_size(size: style.Size, width: Int) -> Int {
  case size {
    style.Px(px) -> px
    style.Pct(pct) -> width * pct / 100
    style.Fill -> width
  }
}

fn calc_size_input(size: style.Size, width: Int, label: String) -> Int {
  calc_size(size, width - string.length(label))
}

type Separator {
  SepRow
  SepCol
}

fn calc_align(text: String, align: style.Align, width: Int) -> String {
  let len = text |> string.length
  calc_align_len(text, align, width, len)
}

fn calc_align_len(
  text: String,
  align: style.Align,
  width: Int,
  len: Int,
) -> String {
  let center = { width / 2 } - { len / 2 }
  case align {
    style.Left -> text
    style.Center -> c(SavePos) <> c(MoveRight(center)) <> text <> c(LoadPos)
    style.Right -> c(SavePos) <> c(MoveRight(width - len)) <> text <> c(LoadPos)
  }
}

fn right_is_left(align: style.Align) -> style.Align {
  case align {
    style.Right -> style.Left
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
  fg: Option(style.Color),
  bg: Option(style.Color),
  pos: Pos,
) -> Element {
  let width = pos.width - 2
  text
  |> string.slice(0, width)
  |> calc_align(pos.align, pos.width)
  |> style_text(fg, bg)
  |> Element(width:, height: 1)
}

fn style_text(
  text: String,
  fg: Option(style.Color),
  bg: Option(style.Color),
) -> String {
  c(Reset)
  <> { option.map(fg, fn(o) { c(Fg(o)) }) |> option.unwrap("") }
  <> { option.map(bg, fn(o) { c(Bg(o)) }) |> option.unwrap("") }
  <> text
  <> c(Reset)
}

fn draw_text_multi(
  text: String,
  fg: Option(style.Color),
  bg: Option(style.Color),
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
    cursor: Int,
    offset: Int,
    hidden: Bool,
  )
}

fn draw_input(input: Iput) -> Element {
  // 2 == padding of space either side
  let color = case input.pressed {
    True -> style.Green |> Fg |> c
    False -> style.Blue |> Fg |> c
  }
  let in_width = input.width - 2
  let text = case input.hidden {
    True -> string.length(input.text) |> string.repeat("•", _)
    False -> input.text
  }
  let text = text <> " "
  let text_trim =
    text
    |> string.slice(input.offset, in_width)
    |> fn(x) {
      case input.pressed {
        True -> map_cursor(x, input.cursor - input.offset, input.width)
        False -> x
      }
    }

  [color, input.title, " ", c(Reset), text_trim]
  |> string.join("")
  |> Element(width: input.width, height: input.height)
}

fn map_cursor(str: String, cursor: Int, width: Int) -> String {
  let pos = cursor |> int.min(width - 2)
  str
  |> string.to_graphemes
  |> list.index_map(fn(i, idx) {
    case idx == pos {
      True -> c(Bg(style.White)) <> c(Fg(style.Black)) <> i <> c(Reset)
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
    align: style.Align,
  )
}

fn draw_btn(btn: Btn) -> Element {
  let button = {
    "  " <> btn.text <> "  "
  }
  let width = string.length(button)
  let button = button |> calc_align(btn.align, btn.width)
  let bg = case btn.pressed {
    False -> style.Blue |> Bg |> c
    True -> style.Green |> Bg |> c
  }
  [bg, c(Fg(style.Black)), button, c(Reset)]
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
  color color: style.Color,
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

fn draw_table(width: Int, values: List(List(String)), pos: Pos) -> Element {
  let width = int.min(width, pos.width)
  let row_count = values |> list.length
  let height = int.min(row_count, pos.height)
  let values = list.take(values, height)
  let col_count =
    values |> list.first |> result.map(list.length) |> result.unwrap(1)
  let col_width = width / col_count
  let col_left_over = width - col_width * col_count
  let start = c(MoveLeft(width)) <> c(MoveDown(1))
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
          c(SGR(Bold))
          <> c(Fg(style.Blue))
          <> row
          <> c(Reset)
          <> c(MoveRight(col_left_over))
          <> start
        _ -> row <> c(MoveRight(col_left_over)) <> start
      }
    }
  }
  let rows = values |> list.index_map(row) |> string.join("")
  let join_offset = [c(MoveUp(1)), c(SavePos)] |> string.join("")
  [c(Reset), rows, join_offset]
  |> string.join("")
  |> Element(width:, height:)
}

fn draw_table_kv(width: Int, values: List(List(String)), pos: Pos) -> Element {
  let width = int.min(width, pos.width)
  let row_count = values |> list.length
  let height = int.min(row_count, pos.height)
  let values = list.take(values, height)
  let col_count =
    values |> list.first |> result.map(list.length) |> result.unwrap(1)
  let col_width = { width } / col_count
  let col_left_over = width - col_width * col_count

  let start = c(MoveLeft(width)) <> c(MoveDown(1))
  let row = fn(row) {
    list.index_map(row, fn(col, idx) {
      let trim = case string.length(col) {
        x if x >= col_width -> string.slice(col, 0, col_width - 3) <> ".. "
        x -> col <> c(MoveRight(col_width - x))
      }
      case idx {
        0 -> c(SGR(Bold)) <> c(Fg(style.Blue)) <> trim <> c(Reset)
        _ -> trim
      }
    })
    |> string.join("")
    |> fn(row) { row <> c(MoveRight(col_left_over)) <> start }
  }
  let rows = values |> list.map(row) |> string.join("")
  let join_offset = [c(MoveUp(1)), c(SavePos)] |> string.join("")
  [c(Reset), rows, join_offset]
  |> string.join("")
  |> Element(width:, height:)
}

fn draw_graph(width: Int, height: Int, values: List(Float)) -> Element {
  let values = list.take(values, width)
  let lhs = string.repeat("│" <> c(MoveLeft(1)) <> c(MoveDown(1)), height)
  let btm = string.repeat("─", width - 1)
  // TODO: remove assert
  let content = {
    use max <- result.map(list.max(values, float.compare))
    let plot =
      values
      |> list.map(fn(value) {
        let height = height - 1
        let pct = value /. max *. 100.0
        let offset = int.to_float(height) *. pct /. 100.0
        let down = int.to_float(height) -. offset
        let d = down |> float.round
        //c(MoveDown(d)) <> value |> float.round |> int.to_string <> c(MoveUp(d))
        c(MoveDown(d)) <> "•" <> c(MoveUp(d))
      })
      |> string.join("")
    let content =
      [
        c(Reset),
        c(SavePos),
        lhs,
        c(MoveUp(1)),
        "╰",
        btm,
        c(LoadPos),
        c(SavePos),
        c(MoveRight(1)),
        plot,
        c(LoadPos),
        // offset a div col join
        c(MoveDown(height - 1)),
        c(SavePos),
        c(Reset),
      ]
      |> string.join("")
    Element(width:, height:, content:)
  }
  case content {
    Ok(content) -> content
    Error(Nil) -> Element(width: 5, height: 1, content: "error finding max")
  }
}

//
// LAYOUT
//

/// Options for defining and controlling application layout.
pub type Layout(msg) {
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
  Grid(
    gap: Int,
    rows: List(style.Size),
    columns: List(style.Size),
    cells: List(Cell(msg)),
  )
}

/// A Cell is an item within a Layout. Use Cells to define how many rows and
/// columns the content should cover.
///
pub type Cell(msg) {
  Cell(content: Node(msg), row: #(Int, Int), col: #(Int, Int))
}

fn layout(layout: Layout(msg), pos: Pos) -> List(#(Node(msg), Pos)) {
  let col_sizes = layout.columns |> calc_sizes(pos.width, _)
  let row_sizes = layout.rows |> calc_sizes(pos.height, _)
  layout.cells
  |> list.map(fn(cell) {
    let #(x, w) = calc_cell_size(layout.gap, cell.col.0, cell.col.1, col_sizes)
    let #(y, h) = calc_cell_size(layout.gap, cell.row.0, cell.row.1, row_sizes)
    #(
      cell.content,
      Pos(x: pos.x + x, y: pos.y + y, width: w, height: h, align: style.Left),
    )
  })
}

fn calc_cell_size(gap: Int, from: Int, to: Int, of: List(Int)) -> #(Int, Int) {
  list.index_fold(of, #(1, 0), fn(acc, item, idx) {
    case idx {
      x if x >= from && x <= to -> #(acc.0, acc.1 + item)
      x if x == from - 1 -> #(acc.0 + gap + item, acc.1 - gap)
      x if x < from -> #(acc.0 + item, acc.1)
      _ -> acc
    }
  })
}

fn calc_sizes(max: Int, sizes: List(style.Size)) -> List(Int) {
  let first =
    list.map(sizes, fn(size) {
      case size {
        style.Px(px) -> Some(px)
        style.Pct(pct) -> Some(max * pct / 100)
        style.Fill -> None
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
  Fg(style.Color)
  Bg(style.Color)
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
    MoveUp(i) -> { esc <> "[" <> int.to_string(i) <> "A" } |> ignore_zero(i)
    MoveDown(i) -> { esc <> "[" <> int.to_string(i) <> "B" } |> ignore_zero(i)
    MoveLeft(i) -> { esc <> "[" <> int.to_string(i) <> "D" } |> ignore_zero(i)
    MoveRight(i) -> { esc <> "[" <> int.to_string(i) <> "C" } |> ignore_zero(i)
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

// TODO: move to style, this should be made public, styling UI elements
// probably needs reworked first.
type Graphic {
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

fn col(color: style.Color) -> String {
  case color {
    style.Black -> "0"
    style.Red -> "1"
    style.Green -> "2"
    style.Yellow -> "3"
    style.Blue -> "4"
    style.Magenta -> "5"
    style.Cyan -> "6"
    style.White -> "7"
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
