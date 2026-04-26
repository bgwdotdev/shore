import gleam/bit_array
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor.{type Started}
import gleam/result
import gleam/string
import shore/effect.{type Effect}
import shore/internal/signal
import shore/key.{type Key}
import shore/style

fn effect_handler(effect: Effect(msg), queue: fn(Event(msg)) -> Nil) -> Nil {
  list.each(effect.to_list(effect), do_effect_handler(_, queue))
}

fn do_effect_handler(
  effect: fn(fn(msg) -> Nil) -> Nil,
  queue: fn(Event(msg)) -> Nil,
) -> Nil {
  effect(fn(msg) {
    spawn_unlinked(fn() { msg |> Cmd |> queue })
    Nil
  })
}

//
// INIT
//

pub type Spec(model, msg) {
  Spec(
    init: fn(fn(msg) -> Nil) -> #(model, Effect(msg)),
    view: fn(model) -> Node(msg),
    update: fn(model, msg) -> #(model, Effect(msg)),
    exit: fn(Nil) -> Nil,
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
    effect_queue: fn(Event(msg)) -> Nil,
    focused: Option(Focused(msg)),
    renderer: fn(String) -> Nil,
    last_frame: String,
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
    submit: Option(msg),
    focus_on: Key,
    cursor: Int,
    offset: Int,
    width: Int,
  )
  FocusedButton(label: String, event: msg)
}

//
// START
//

pub fn start(
  spec: Spec(model, msg),
) -> Result(fn(Event(msg)) -> Nil, StartError) {
  start_custom_renderer(spec, None)
}

pub fn start_custom_renderer(
  spec: Spec(model, msg),
  terminal_handler: Option(TerminalHandler(msg)),
) -> Result(fn(Event(msg)) -> Nil, StartError) {
  let terminal_handler =
    option.unwrap(terminal_handler, default_terminal_handler())
  use shore <- result.map(shore_start(spec, terminal_handler))
  redraw_on_timer(spec, shore)
  shore
}

pub type StartError {
  StartError
}

@target(javascript)
fn shore_start(
  spec: Spec(model, msg),
  terminal_handler: TerminalHandler(msg),
) -> Result(fn(Event(msg)) -> Nil, StartError) {
  let state = fn(effect_queue: fn(Event(msg)) -> Nil) {
    let #(width, height) = terminal_handler.init()
    terminal_handler.input(effect_queue)
    terminal_handler.resize(effect_queue)
    let #(model, effect_init) =
      spec.init(fn(msg) { msg |> Cmd |> effect_queue })
    let state =
      State(
        spec:,
        model:,
        width:,
        height:,
        effect_queue:,
        focused: None,
        renderer: terminal_handler.draw,
        last_frame: "",
      )
    effect_handler(effect_init, effect_queue)
    let state = model |> spec.view |> render(state, _, key.Null)
    state
  }
  let loop = fn(state, event) {
    case shore_loop(state, event) {
      Ok(state) -> state
      Error(Nil) -> {
        exit_js()
        state
      }
    }
  }
  let shore = start_js(state, loop)
  let callback = fn(event) { send_js(shore, event) }
  Ok(callback)
}

@external(javascript, "./internal.ffi.mjs", "exit")
fn exit_js() -> Nil {
  Nil
}

type Shore

@external(javascript, "./internal.ffi.mjs", "start")
fn start_js(
  spec: fn(fn(Event(msg)) -> Nil) -> State(model, msg),
  loop: fn(State(model, msg), Event(msg)) -> State(model, msg),
) -> Shore

@external(javascript, "./internal.ffi.mjs", "send")
fn send_js(shore: Shore, event: Event(msg)) -> Nil

@target(erlang)
fn shore_start(
  spec: Spec(model, msg),
  terminal_handler: TerminalHandler(msg),
) -> Result(fn(Event(msg)) -> Nil, StartError) {
  actor.new_with_initialiser(1000, fn(effect_subject) {
    let effect_queue: fn(Event(msg)) -> Nil = fn(effect) {
      effect |> process.send(effect_subject, _)
    }
    let subj = process.new_subject()
    let #(model, effect_init) = spec.init(fn(msg) { process.send(subj, msg) })
    let #(width, height) = terminal_handler.init()
    terminal_handler.input(effect_queue)
    terminal_handler.resize(effect_queue)
    let state =
      State(
        spec:,
        model:,
        width:,
        height:,
        effect_queue:,
        focused: None,
        renderer: terminal_handler.draw,
        last_frame: "",
      )
    let state = model |> spec.view |> render(state, _, key.Null)
    effect_handler(effect_init, effect_queue)

    let selector =
      process.new_selector()
      |> process.select(effect_subject)
      |> process.select_map(subj, Cmd)

    state
    |> actor.initialised
    |> actor.selecting(selector)
    |> actor.returning(effect_subject)
    |> Ok
  })
  |> actor.on_message(fn(state, event) {
    case shore_loop(state, event) {
      Ok(state) -> actor.continue(state)
      Error(Nil) -> {
        process.sleep(16)
        actor.stop()
      }
    }
  })
  |> actor.start
  |> result.replace_error(StartError)
  |> result.map(fn(actor) {
    fn(event) {
      let actor.Started(data: shore, ..) = actor
      actor.send(shore, event)
    }
  })
}

//
// TERMINAL HANDLER
//

pub opaque type TerminalHandler(msg) {
  TerminalHandler(
    /// handle drawing the view to the terminal
    draw: fn(String) -> Nil,
    /// setup the terminal and return the initial width/height of terminal
    init: fn() -> #(Int, Int),
    /// handle key input and send event to runtime
    input: fn(fn(Event(msg)) -> Nil) -> Nil,
    /// handle terminal resize and send event to runtime
    resize: fn(fn(Event(msg)) -> Nil) -> Nil,
  )
}

@target(javascript)
fn default_terminal_handler() -> TerminalHandler(msg) {
  let draw = fn(view) { io.print(view) }
  let init = fn() {
    let assert Ok(width) = terminal_columns()
    let assert Ok(height) = terminal_rows()
    raw()
    draw(init_terminal())
    #(width, height)
  }
  let input = fn(effect_queue) { on_input(send_input(_, effect_queue)) }
  let resize = fn(effect_queue) {
    case is_windows() {
      True ->
        fn(state) { resize_poll(effect_queue, state) }
        |> on_interval(#(0, 0), 16)
      False -> resize_sigwinch(effect_queue)
    }
  }
  TerminalHandler(draw:, init:, resize:, input:)
}

@target(erlang)
fn default_terminal_handler() -> TerminalHandler(msg) {
  let assert Ok(renderer) =
    actor.new(Nil) |> actor.on_message(default_renderer_loop) |> actor.start
  let draw = fn(view) { process.send(renderer.data, view) }
  let init = fn() {
    let assert Ok(width) = terminal_columns()
    let assert Ok(height) = terminal_rows()
    raw()
    draw(init_terminal())
    #(width, height)
  }
  let input = fn(effect_queue) {
    spawn(fn() { on_input(send_input(_, effect_queue)) })
  }
  let resize = fn(effect_queue) {
    case is_windows() {
      True ->
        fn(state) { resize_poll(effect_queue, state) }
        |> on_interval(#(0, 0), 16)
      False -> resize_sigwinch(effect_queue)
    }
  }
  TerminalHandler(draw:, init:, input:, resize:)
}

fn default_renderer_loop(state: Nil, msg: String) -> actor.Next(Nil, String) {
  msg |> io.print
  actor.continue(state)
}

//
// RESIZE
//

fn resize_poll(
  effect_queue: fn(Event(msg)) -> Nil,
  size: #(Int, Int),
) -> #(Int, Int) {
  let assert Ok(width) = terminal_columns()
  let assert Ok(height) = terminal_rows()
  let new_size = #(width, height)
  let size = case size == new_size {
    True -> size
    False -> {
      effect_queue(Resize(new_size.0, new_size.1))
      effect_queue(Redraw)
      new_size
    }
  }
  size
}

fn resize_sigwinch(effect_queue: fn(Event(msg)) -> Nil) -> Nil {
  fn() {
    let assert Ok(width) = terminal_columns()
    let assert Ok(height) = terminal_rows()
    let resize = Resize(width, height)
    effect_queue(resize)
    effect_queue(Redraw)
  }
  |> signal.start
}

//
// READ INPUT
//

/// note: this is blocking on javascript, prefer on_input
@external(erlang, "io", "get_chars")
@external(javascript, "./internal.ffi.mjs", "get_chars")
fn get_chars(prompt: String, count: Int) -> String

@external(javascript, "./internal.ffi.mjs", "on_input")
fn on_input(fun: fn(String) -> Nil) -> Nil {
  get_chars("", 1024) |> fun
  on_input(fun)
}

fn send_input(key: String, effect_queue: fn(Event(msg)) -> Nil) -> Nil {
  key |> key.from_string |> KeyPress |> effect_queue
}

//
// EVENT
//

/// Events can be sent to the subject of the shore actor. Use the `cmd` and `exit` functions.
pub opaque type Event(msg) {
  KeyPress(Key)
  Cmd(msg)
  Redraw
  Resize(width: Int, height: Int)
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

pub fn key_press(key: Key) -> Event(msg) {
  KeyPress(key)
}

pub fn resize(width width: Int, height height: Int) -> Event(msg) {
  Resize(width:, height:)
}

fn shore_loop(
  state: State(model, msg),
  event: Event(msg),
) -> Result(State(model, msg), Nil) {
  // NOTE: assign here to avoid syntax highlighting error, delete whenever fixed
  let exit = state.spec.keybinds.exit

  case event {
    Cmd(msg) -> {
      let #(model, effect) = state.spec.update(state.model, msg)
      effect_handler(effect, state.effect_queue)
      let state = State(..state, model: model)
      let state = redraw_on_update(state, key.Null)
      state |> Ok
    }
    KeyPress(input) if input == exit -> shore_loop(state, Exit)
    KeyPress(input) -> {
      let ui = state.spec.view(state.model)
      let state = case state.focused {
        // not focused
        None -> {
          let focusable = list_focusable([ui], state)
          // check application focus keybind
          let state = case focus_keybind(focusable, input) {
            Some(..) as focused -> State(..state, focused:)
            _ -> state
          }
          // check for application button keybind
          let model = case detect_event(state, ui, input) {
            Some(msg) -> {
              let #(model, effect) = state.spec.update(state.model, msg)
              effect_handler(effect, state.effect_queue)
              model
            }
            None -> state.model
          }
          State(..state, model:)
        }
        // focused
        Some(focused) -> {
          // progress focus state
          let reload = list_focusable([ui], state) |> focus_current(focused)
          case reload {
            None -> State(..state, focused: None)
            Some(focused) -> {
              case input_handler(focused, input) {
                FocusedInput(..) as focused -> {
                  case input == state.spec.keybinds.submit, focused.submit {
                    True, Some(event) -> {
                      let #(model, effect) =
                        state.spec.update(state.model, event)
                      let reload =
                        list_focusable([state.spec.view(model)], state)
                        |> focus_current(focused)
                      effect_handler(effect, state.effect_queue)
                      State(..state, focused: reload, model:)
                    }
                    _, _ -> {
                      let #(model, effect) =
                        state.spec.update(
                          state.model,
                          focused.event(focused.value),
                        )
                      effect_handler(effect, state.effect_queue)
                      State(..state, focused: Some(focused), model:)
                    }
                  }
                }
                FocusedButton(..) as focused -> {
                  case input == state.spec.keybinds.submit {
                    True -> {
                      let #(model, effect) =
                        state.spec.update(state.model, focused.event)
                      let reload =
                        list_focusable([state.spec.view(model)], state)
                        |> focus_current(focused)
                      effect_handler(effect, state.effect_queue)
                      State(..state, focused: reload, model:)
                    }
                    False -> state
                  }
                }
              }
            }
          }
        }
      }

      // check global focus keybind
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
      let state = redraw_on_update(state, input)
      state |> Ok
    }
    Redraw -> redraw(state, key.Null) |> Ok
    Resize(width:, height:) -> State(..state, width:, height:) |> Ok
    Exit -> {
      state.renderer(restore_terminal())
      state.spec.exit(Nil)
      Error(Nil)
    }
  }
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
    KeyBind(key:, event:) if input == key -> Some(event)
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
    | TextMulti(..)
    | KittyGraphic(..)
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

fn redraw_on_timer(
  spec: Spec(model, msg),
  effect_queue: fn(Event(msg)) -> Nil,
) -> Nil {
  case spec.redraw {
    OnUpdate -> Nil
    OnTimer(timer) -> {
      fn(_) {
        effect_queue(Redraw)
        Nil
      }
      |> on_interval(Nil, timer)
      Nil
    }
  }
}

fn redraw(state: State(model, msg), input: Key) -> State(model, msg) {
  state.model |> state.spec.view |> render(state, _, input)
}

fn redraw_on_update(state: State(model, msg), input: Key) -> State(model, msg) {
  case state.spec.redraw {
    OnUpdate -> redraw(state, input)
    OnTimer(_) -> state
  }
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
        Aligned(node:, ..) ->
          do_list_focusable(pos, xs, do_list_focusable(pos, [node], acc))
        Layouts(l) -> {
          layout(l, pos)
          |> list.map(fn(i) { do_list_focusable(i.1, [i.0], acc) })
          |> list.reverse
          |> list.flatten
        }
        Input(label:, value:, width:, event:, submit:, hidden: _, focus_on:) -> {
          let cursor = string.length(value)
          let width = calc_size_input(width, pos.width, label)
          let focused =
            FocusedInput(
              label:,
              value:,
              event:,
              submit:,
              focus_on:,
              offset: 0,
              cursor:,
              width:,
            )
          let offset = input_offset(cursor, focused.offset, focused.width)
          do_list_focusable(pos, xs, [FocusedInput(..focused, offset:), ..acc])
        }
        Button(id:, event:, ..) -> {
          do_list_focusable(pos, xs, [FocusedButton(id, event), ..acc])
        }
        BR
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
        | KittyGraphic(..)
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
        False -> focus_next(xs, focused, x.label == focused.label)
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
        True ->
          case x, focused {
            FocusedInput(..) as new, FocusedInput(..) as old ->
              case old.cursor > string.length(new.value) {
                True ->
                  FocusedInput(
                    ..new,
                    cursor: string.length(new.value),
                    offset: 0,
                  )
                False ->
                  FocusedInput(..new, cursor: old.cursor, offset: old.offset)
              }

            new, _old -> new
          }
          |> Some
        False -> focus_current(xs, focused)
      }
  }
}

fn focus_keybind(
  focusable: List(Focused(msg)),
  input: Key,
) -> Option(Focused(msg)) {
  case focusable {
    [] -> None
    [x, ..xs] ->
      case x {
        FocusedInput(focus_on:, ..) if focus_on == input -> Some(x)
        _ -> focus_keybind(xs, input)
      }
  }
}

//
// TEXT INPUT
//

// TODO: rewrite all this into a zipper: https://en.wikipedia.org/wiki/Zipper_(data_structure)

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
        key.CtrlLeft -> {
          let cursor =
            int.max(0, string_ctrl_left(focused.cursor - 1, focused.value))
          let offset = input_offset(cursor, focused.offset, focused.width)
          FocusedInput(..focused, cursor:, offset:)
        }
        key.CtrlRight -> {
          let len = string.length(focused.value)
          let cursor =
            int.min(
              len,
              string_ctrl_right(focused.cursor + 1, focused.value, len),
            )
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
        key.CtrlDelete -> {
          let value = string_ctrl_delete(focused.value, focused.cursor)
          FocusedInput(..focused, value:)
        }
        key.CtrlBackspace -> {
          let #(cursor, value) =
            string_ctrl_backspace(focused.value, focused.cursor)
          FocusedInput(..focused, cursor:, value:)
        }
        key.Char(char) -> {
          let cursor = focused.cursor + string.length(char)
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
  // padding is due to the width of an input field having space either side, plus additional one for the cursor itself, this is probably a pretty dumb implementation ^^;
  let padding = 3
  case cursor {
    x if x < offset -> cursor
    x if x >= offset + { width - padding } -> cursor - width + padding
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

fn string_ctrl_delete(str: String, cursor: Int) -> String {
  str
  |> bit_array.from_string
  |> do_string_ctrl_delete(cursor, 0, <<>>, <<>>)
  |> bit_array.to_string
  |> result.unwrap(str)
}

@target(javascript)
fn do_string_ctrl_delete(
  str: BitArray,
  cursor: Int,
  idx: Int,
  acc: BitArray,
  to_delete: BitArray,
) -> BitArray {
  todo as "implement zipper"
}

@target(erlang)
fn do_string_ctrl_delete(
  str: BitArray,
  cursor: Int,
  idx: Int,
  acc: BitArray,
  to_delete: BitArray,
) -> BitArray {
  case str {
    <<char:utf8_codepoint, rest:bits>> ->
      case idx >= cursor {
        True ->
          case <<char:utf8_codepoint>>, string_is_spaces(to_delete) {
            <<" ":utf8>>, False -> <<acc:bits, " ":utf8, rest:bits>>
            _, _ ->
              do_string_ctrl_delete(rest, cursor, idx + 1, acc, <<
                to_delete:bits,
                char:utf8_codepoint,
              >>)
          }
        False ->
          do_string_ctrl_delete(
            rest,
            cursor,
            idx + 1,
            <<
              acc:bits,
              char:utf8_codepoint,
            >>,
            to_delete,
          )
      }
    _ -> acc
  }
}

fn string_ctrl_backspace(str: String, cursor: Int) -> #(Int, String) {
  let #(cursor, new_str) =
    str
    |> bit_array.from_string
    |> do_string_ctrl_backspace(cursor, 0, <<>>, <<>>, 0)
  #(
    cursor,
    new_str
      |> bit_array.to_string
      |> result.unwrap(str),
  )
}

@target(javascript)
fn do_string_ctrl_backspace(
  str: BitArray,
  cursor: Int,
  idx: Int,
  acc: BitArray,
  to_delete: BitArray,
  to_delete_idx: Int,
) -> #(Int, BitArray) {
  todo as "implement zipper"
}

@target(erlang)
fn do_string_ctrl_backspace(
  str: BitArray,
  cursor: Int,
  idx: Int,
  acc: BitArray,
  to_delete: BitArray,
  to_delete_idx: Int,
) -> #(Int, BitArray) {
  case str {
    <<char:utf8_codepoint, rest:bits>> ->
      case idx + 1 >= cursor {
        True -> #(to_delete_idx, word_concat(acc, rest))
        False ->
          case <<char:utf8_codepoint>> {
            <<" ":utf8>> -> {
              let acc = word_concat(acc, to_delete)
              do_string_ctrl_backspace(rest, cursor, idx + 1, acc, <<>>, idx)
            }
            _ -> {
              let to_delete = <<
                to_delete:bits,
                char:utf8_codepoint,
              >>
              do_string_ctrl_backspace(
                rest,
                cursor,
                idx + 1,
                acc,
                to_delete,
                to_delete_idx,
              )
            }
          }
      }
    _ -> #(cursor, acc)
  }
}

fn word_concat(left: BitArray, right: BitArray) -> BitArray {
  case left {
    <<>> -> right
    _ -> <<left:bits, " ":utf8, right:bits>>
  }
}

fn string_is_spaces(str: BitArray) -> Bool {
  case str {
    <<" ":utf8, rest:bits>> -> string_is_spaces(rest)
    <<>> -> True
    _ -> False
  }
}

fn string_ctrl_left(idx: Int, str: String) -> Int {
  case idx < 0 {
    True -> 0
    False ->
      case string.slice(str, idx, 1) {
        " " -> idx
        _ -> string_ctrl_left(idx - 1, str)
      }
  }
}

fn string_ctrl_right(idx: Int, str: String, len: Int) -> Int {
  case idx >= len {
    True -> idx
    False ->
      case string.slice(str, idx, 1) {
        " " -> idx
        _ -> string_ctrl_right(idx + 1, str, len)
      }
  }
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

type Pos {
  Pos(x: Int, y: Int, width: Int, height: Int, align: style.Align)
}

fn render(
  state: State(model, msg),
  node: Node(msg),
  last_input: Key,
) -> State(model, msg) {
  let pos = Pos(0, 0, state.width, state.height, style.Left)
  let frame =
    c(Clear)
    <> node
    |> render_node(state.focused, _, last_input, pos)
    |> option.map(fn(r) { r.content })
    |> option.unwrap("")
  let render = [c(BSU), frame, c(ESU)]
  case frame != state.last_frame {
    True -> {
      render |> list.each(state.renderer)
      State(..state, last_frame: frame)
    }
    False -> state
  }
}

pub fn render_static(
  model: model,
  view: fn(model) -> Node(msg),
  width: style.Size,
  height: style.Size,
) -> String {
  raw()
  c(GetPos) |> io.println
  let #(row, col) = get_chars("", 1024) |> parse_position
  let assert Ok(term_width) = terminal_columns()
  let assert Ok(term_height) = terminal_rows()
  let width = calc_size(width, term_width)
  let height = calc_size(height, term_height)
  // adjusts the window depending if content would overflow the terminal
  let #(scroll_up, row) = case { row + height } > term_height {
    True -> #(c(ScrollUp(height)), row - height)
    False -> #("", row)
  }
  let pos = Pos(col, row, width, height, style.Left)
  let focused = None
  let last_input = key.Null
  let node = view(model)
  let render =
    render_node(focused, node, last_input, pos)
    |> option.map(fn(r) { r.content })
    |> option.unwrap("")
  [
    scroll_up,
    render,
  ]
  |> string.concat
}

fn parse_position(str: String) -> #(Int, Int) {
  let pos =
    str
    |> string.replace("\u{001B}[", "")
    |> string.replace("R", "")
    |> string.split(";")
    |> list.map(int.parse)
    |> result.all
  case pos {
    Ok([row, col]) -> #(row, col)
    _ -> #(0, 0)
  }
}

fn render_node(
  focused: Option(Focused(msg)),
  node: Node(msg),
  last_input: Key,
  pos: Pos,
) -> Option(Element) {
  case node {
    Layouts(l) -> {
      layout(l, pos)
      |> list.map(fn(i) {
        let cursor = c(SetPos({ i.1 }.y, { i.1 }.x))
        render_node(focused, i.0, last_input, i.1)
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
      render_node(focused, node, last_input, Pos(..pos, align:))
    Row(children) -> {
      let len = children |> list.length
      let width = pos.width / len
      list.index_map(children, fn(child, idx) {
        let x = idx * width
        let new_pos = Pos(..pos, x:, width:, align: right_is_left(pos.align))
        render_node(focused, child, last_input, new_pos)
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
      list.map(children, render_node(focused, _, last_input, pos))
      |> option.values
      |> element_join(sep(SepCol))
      |> element_prefix(c(SavePos))
      |> Some
    }
    Box(children:, title:, fg:) -> {
      // TODO: review how box grid gaps implement
      let pos = Pos(..pos, width: pos.width - 3, height: pos.height - 2)
      let pos_child = Pos(..pos, width: pos.width - 2)
      [
        draw_box(int.max(pos.width, 1), int.max(pos.height, 1), title, fg),
        ..list.map(children, render_node(focused, _, last_input, pos_child))
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
    Button(id:, text:, key:, event: _, fg:, bg:, focus_fg:, focus_bg:) -> {
      let is_focused = case focused {
        Some(FocusedButton(..) as focused) if focused.label == id -> True
        _ -> False
      }
      draw_btn(Btn(
        width: pos.width,
        height: 1,
        text:,
        pressed: last_input == key || is_focused,
        align: pos.align,
        fg:,
        bg:,
        focus_fg:,
        focus_bg:,
      ))
      |> Some
    }
    KeyBind(..) -> None
    Input(label:, value:, width:, event: _, submit: _, hidden:, focus_on: _) -> {
      let width = calc_size_input(width, pos.width, label)
      let #(is_focused, cursor) = case focused {
        Some(FocusedInput(..) as focused) if focused.label == label -> #(
          True,
          focused.cursor,
        )
        _ -> #(False, string.length(value))
      }
      let offset = case focused {
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
    TextMulti(text:, wrap:, fg:, bg:, graphics:) ->
      draw_text_multi(text, wrap, fg, bg, graphics, pos) |> Some

    HR ->
      { c(Reset) <> string.repeat("─", pos.width) }
      |> Element(width: pos.width, height: 1)
      |> Some
    HR2(color) ->
      { c(Reset) <> c(Fg(color)) <> string.repeat("─", pos.width) <> c(Reset) }
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
      render_node(focused, node, last_input, pos)
      |> option.map(element_prefix(_, bar))
    }
    BR -> "\n" |> Element(width: pos.width, height: 1) |> Some
    Progress(width:, max:, value:, color:) -> {
      let width = calc_size(width, pos.width)
      draw_progress(width:, max:, value:, color:, pos:) |> Some
    }
    KittyGraphic(payload:, width:, height:) ->
      draw_kitty_graphic(
        payload,
        calc_size(width, pos.width),
        calc_size(height, pos.height),
      )
      |> Some
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
    submit: Option(msg),
    hidden: Bool,
    focus_on: Key,
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
  /// A multi-line text string
  TextMulti(
    text: String,
    wrap: TextWrap,
    fg: Option(style.Color),
    bg: Option(style.Color),
    graphics: List(style.Graphic),
  )
  /// A button assigned to a key press to execute an event
  Button(
    id: String,
    text: String,
    key: Key,
    event: msg,
    fg: Option(style.Color),
    bg: Option(style.Color),
    focus_fg: Option(style.Color),
    focus_bg: Option(style.Color),
  )
  /// A non-visible button assigned to a key press to execute an event
  KeyBind(key: Key, event: msg)
  /// Sets alignment of all child nodes
  Aligned(align: style.Align, node: Node(msg))
  /// A container element for holding other nodes over multiple lines
  Col(children: List(Node(msg)))
  /// A container element for holding other nodes in a single line
  Row(children: List(Node(msg)))
  /// A box container element for holding other nodes
  Box(children: List(Node(msg)), title: Option(String), fg: Option(style.Color))
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
  /// A base64 image using kitty graphics protocol
  KittyGraphic(payload: String, width: style.Size, height: style.Size)
}

/// finds the absolute size on a node
fn calc_size(size: style.Size, width: Int) -> Int {
  case size {
    style.Px(px) -> px
    style.Pct(pct) -> width * pct / 100
    style.Fill -> width
    style.MinMax(min:, max:) -> {
      let max = calc_size(max, width)
      let min = calc_size(min, width)
      case max {
        _ if max > width -> min
        _ if min > width -> min
        _ if max > min -> max
        _ if min > max -> max
        _ -> min
      }
    }
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

fn style_text(
  text: String,
  fg: Option(style.Color),
  bg: Option(style.Color),
  graphics: List(style.Graphic),
) -> String {
  let foreground = fg |> option.map(Fg) |> option.map(c) |> option.unwrap("")
  let background = bg |> option.map(Bg) |> option.map(c) |> option.unwrap("")
  let graphics = graphics |> list.map(SGR) |> list.map(c) |> string.join("")
  c(Reset) <> foreground <> background <> graphics <> text <> c(Reset)
}

pub type TextWrap {
  Wrap
  NoWrap
}

fn draw_text_multi(
  text: String,
  wrap: TextWrap,
  fg: Option(style.Color),
  bg: Option(style.Color),
  graphics: List(style.Graphic),
  pos: Pos,
) -> Element {
  let width = pos.width - 2
  let height = pos.height
  let text =
    c(SavePos)
    <> {
      text
      |> text_wrap(wrap, width - 1)
      |> list.take(height)
      |> list.map(string.slice(_, 0, width))
      |> list.map(calc_align(_, pos.align, pos.width))
      |> string.join(c(LoadPos) <> c(MoveDown(1)) <> c(SavePos))
    }
    <> c(LoadPos)
    <> c(MoveDown(1))
  text
  |> style_text(fg, bg, graphics)
  |> Element(width:, height:)
}

fn text_wrap(text: String, wrap: TextWrap, width: Int) -> List(String) {
  case wrap {
    Wrap -> text_wrap_loop(string.to_graphemes(text), width, 0, 0, [], [], [])
    NoWrap -> text |> string.split("\n")
  }
}

fn text_wrap_loop(
  text: List(String),
  width: Int,
  line_length: Int,
  word_length: Int,
  word: List(String),
  line: List(String),
  acc: List(String),
) -> List(String) {
  let append = fn(word, line, acc) {
    let line = list.append(word, line) |> list.reverse |> string.join("")
    [line, ..acc]
  }
  case text {
    // respect \n's
    ["\n", ..xs] ->
      text_wrap_loop(xs, width, 0, 0, [], [], append(word, line, acc))
    // append word to line
    [" ", ..xs] ->
      text_wrap_loop(
        xs,
        width,
        line_length + 1,
        0,
        [],
        list.append([" ", ..word], line),
        acc,
      )
    [x, ..xs] ->
      case line_length >= width, word_length >= width {
        // word length exceeds entire width of viewport, split word
        _, True ->
          text_wrap_loop(
            xs,
            width,
            0,
            0,
            [],
            [],
            append([x, ..word], line, acc),
          )
        // line length exceeds width, split line
        True, _ ->
          text_wrap_loop(
            list.append(list.reverse([x, ..word]), xs),
            width,
            0,
            0,
            [],
            [],
            append([], line, acc),
          )
        // build line
        _, _ ->
          text_wrap_loop(
            xs,
            width,
            line_length + 1,
            word_length + 1,
            [x, ..word],
            line,
            acc,
          )
      }
    // return wrapped text
    [] -> append(word, line, acc) |> list.reverse
  }
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

  [c(Reset), color, input.title, " ", c(Reset), text_trim]
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
    text: String,
    pressed: Bool,
    align: style.Align,
    fg: Option(style.Color),
    bg: Option(style.Color),
    focus_fg: Option(style.Color),
    focus_bg: Option(style.Color),
  )
}

fn draw_btn(btn: Btn) -> Element {
  let button = {
    "  " <> btn.text <> "  "
  }
  let width = string.length(button)
  let button = button |> calc_align(btn.align, btn.width)
  let bg = case btn.pressed {
    False -> btn.bg
    True -> btn.focus_bg
  }
  let fg = case btn.pressed {
    False -> btn.fg
    True -> btn.focus_fg
  }
  button
  |> style_text(fg, bg, [])
  |> Element(width:, height: 1)
}

fn draw_box(
  width: Int,
  height: Int,
  title: Option(String),
  fg: Option(style.Color),
) -> Element {
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
  |> style_text(fg, None, [])
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
  let complete = { progress * width / 100 } |> int.clamp(0, width)
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
          c(SGR(style.Bold))
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
        0 -> c(SGR(style.Bold)) <> c(Fg(style.Blue)) <> trim <> c(Reset)
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

fn draw_kitty_graphic(payload: String, width: Int, height: Int) -> Element {
  let content = KittyGraphics(PNG, False, payload, width, height) |> c
  Element(width:, height:, content:)
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
        style.Fill -> None
        size -> calc_size(size, max) |> Some
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
  ScrollUp(Int)
  ScrollDown(Int)
  MoveUp(Int)
  MoveDown(Int)
  MoveLeft(Int)
  MoveRight(Int)
  StartLine
  Column(Int)
  Fg(style.Color)
  Bg(style.Color)
  SGR(style.Graphic)
  Reset
  GetPos
  AltBuffer
  MainBuffer
  /// Begin Synchronized Output
  BSU
  /// End Synchronized Output
  ESU
  KittyGraphics(
    format: KittyFormat,
    compress: Bool,
    payload: String,
    width: Int,
    height: Int,
  )
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
    ScrollUp(i) -> esc <> "[" <> int.to_string(i) <> "S"
    ScrollDown(i) -> esc <> "[" <> int.to_string(i) <> "T"
    MoveUp(i) -> { esc <> "[" <> int.to_string(i) <> "A" } |> ignore_zero(i)
    MoveDown(i) -> { esc <> "[" <> int.to_string(i) <> "B" } |> ignore_zero(i)
    MoveLeft(i) -> { esc <> "[" <> int.to_string(i) <> "D" } |> ignore_zero(i)
    MoveRight(i) -> { esc <> "[" <> int.to_string(i) <> "C" } |> ignore_zero(i)
    StartLine -> Column(1) |> c
    Column(i) -> esc <> "[" <> int.to_string(i) <> "G"
    Fg(color) -> esc <> "[" <> color_to_code(ColorForeground, color) <> "m"
    Bg(color) -> esc <> "[" <> color_to_code(ColorBackground, color) <> "m"
    SGR(graphic) -> esc <> "[" <> graphic_to_string(graphic) <> "m"
    Reset -> esc <> "[0m"
    GetPos -> esc <> "[6n"
    AltBuffer -> esc <> "[?1049h"
    MainBuffer -> esc <> "[?1049l"
    BSU -> esc <> "[?2026h"
    ESU -> esc <> "[?2026l"
    KittyGraphics(format:, compress:, payload:, width:, height:) ->
      payload
      |> kitty_payload([])
      |> kitty_code(format, compress, _, width, height, "")
  }
}

// zero may act like one for move commands for certain terminal emulators
fn ignore_zero(code: String, i: Int) -> String {
  case i {
    0 -> ""
    _ -> code
  }
}

pub fn init_terminal() -> String {
  c(HideCursor) <> c(AltBuffer)
}

pub fn restore_terminal() -> String {
  c(ShowCursor) <> c(MainBuffer)
}

//
// KITTY GRAPHICS PROTOCOL
//

type KittyAction {
  //Transmit
  TransmitAndDisplay
  //Query
  //Put
  //Delete
  //TransmitAnimation
  //ControlAnimation
  //ComposeAnimation
}

fn kitty_action(a: KittyAction) -> String {
  "a="
  <> case a {
    TransmitAndDisplay -> "T"
  }
}

type KittyFormat {
  RGB
  RGBA
  PNG
}

fn kitty_format(f: KittyFormat) -> String {
  "f="
  <> case f {
    RGB -> "24"
    RGBA -> "32"
    PNG -> "100"
  }
}

fn kitty_payload(base64: String, acc: List(String)) -> List(String) {
  let size = 4096
  case string.length(base64) {
    len if len > size -> {
      let chunk = string.slice(base64, 0, size)
      let base64 = string.slice(base64, size, len)
      kitty_payload(base64, [chunk, ..acc])
    }
    _ -> list.reverse([base64, ..acc])
  }
}

fn kitty_code(
  format: KittyFormat,
  compress: Bool,
  payload: List(String),
  width: Int,
  height: Int,
  acc: String,
) -> String {
  let wrap = fn(m, payload) {
    esc
    <> "_G"
    <> kitty_action(TransmitAndDisplay)
    <> ","
    <> kitty_format(format)
    <> ","
    <> m
    <> ",r="
    <> int.to_string(height)
    <> ",c="
    <> int.to_string(width)
    <> ";"
    <> payload
    <> esc
    <> "\\"
  }
  case payload {
    [] -> acc
    [x] ->
      kitty_code(format, compress, [], width, height, acc <> wrap("m=0", x))
    [x, ..xs] ->
      kitty_code(format, compress, xs, width, height, acc <> wrap("m=1", x))
  }
}

//
// GRAPHIC
//

fn graphic_to_string(graphic: style.Graphic) -> String {
  case graphic {
    style.Bold -> "1"
    style.Faint -> "2"
    style.Italic -> "3"
    style.Underline -> "4"
    style.Inverse -> "7"
    style.Conceal -> "8"
    style.Strikethrough -> "9"
  }
}

//
// COLOR
//

type ColorLayer {
  ColorForeground
  ColorBackground
}

fn color_to_code(layer: ColorLayer, color: style.Color) -> String {
  let layer = case layer {
    ColorForeground -> 30
    ColorBackground -> 40
  }
  let color = case color {
    style.Black -> 0
    style.Red -> 1
    style.Green -> 2
    style.Yellow -> 3
    style.Blue -> 4
    style.Magenta -> 5
    style.Cyan -> 6
    style.White -> 7
    style.BrightBlack -> 60
    style.BrightRed -> 61
    style.BrightGreen -> 62
    style.BrightYellow -> 63
    style.BrightBlue -> 64
    style.BrightMagenta -> 65
    style.BrightCyan -> 66
    style.BrightWhite -> 67
  }
  layer + color |> int.to_string
}

//
// SHELL
//

@external(javascript, "./internal.ffi.mjs", "raw")
fn raw() -> Nil {
  raw_ffi(#(Noshell, Raw))
  Nil
}

type ShellOpt {
  Noshell
  Raw
}

type DoNotLeak

@external(erlang, "shell", "start_interactive")
fn raw_ffi(opts: #(ShellOpt, ShellOpt)) -> DoNotLeak

@external(erlang, "io", "rows")
@external(javascript, "./internal.ffi.mjs", "terminal_rows")
fn terminal_rows() -> Result(Int, Nil)

@external(erlang, "io", "columns")
@external(javascript, "./internal.ffi.mjs", "terminal_columns")
fn terminal_columns() -> Result(Int, Nil)

//
// OS
//

pub type Os {
  Unix(name: Atom)
  Win32(name: Atom)
}

@external(erlang, "os", "type")
fn os_type() -> Os

@external(javascript, "./internal.ffi.mjs", "is_windows")
fn is_windows() -> Bool {
  case os_type() {
    Win32(..) -> True
    _ -> False
  }
}

//
// WORK
//

// TODO: javascript spawn not necessary?
@external(javascript, "./internal.ffi.mjs", "spawn")
fn spawn_unlinked(fun: fn() -> msg) -> Nil {
  let _ = process.spawn_unlinked(fun)
  Nil
}

@external(javascript, "./internal.ffi.mjs", "spawn")
fn spawn(fun: fn() -> msg) -> Nil {
  let _ = process.spawn(fun)
  Nil
}

@external(javascript, "./internal.ffi.mjs", "sleep")
fn sleep(fun: fn() -> Nil, duration: Int) -> Nil {
  process.sleep(duration)
  fun()
}

@external(javascript, "./internal.ffi.mjs", "on_interval")
fn on_interval(fun: fn(state) -> state, state: state, interval: Int) -> Nil {
  process.spawn(fn() { do_on_interval(fun, state, interval) })
  Nil
}

@external(javascript, "./internal.ffi.mjs", "on_interval")
fn do_on_interval(fun: fn(state) -> state, state: state, interval: Int) -> Nil {
  let state = fun(state)
  process.sleep(interval)
  do_on_interval(fun, state, interval)
}
