import gleam/int
import gleam/list
import gleam/string
import gleam/string_tree

// reference:
// https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf
// https://learnyouahaskell.github.io/zippers.html

//
// ZIPPER
//

pub opaque type Zipper {
  Zipper(left: List(String), right: List(String))
}

pub fn from_string(str: String) -> Zipper {
  let left = str |> string.to_graphemes() |> list.reverse
  let right = []
  Zipper(left:, right:)
}

pub fn move_left(zipper: Zipper) -> Zipper {
  case zipper.left {
    [first, ..rest] -> Zipper(left: rest, right: [first, ..zipper.right])
    [] -> zipper
  }
}

pub fn move_left_word(zipper: Zipper) -> Zipper {
  case zipper.left {
    [" ", ..rest] -> Zipper(left: rest, right: [" ", ..zipper.right])
    [first, ..rest] ->
      Zipper(left: rest, right: [first, ..zipper.right]) |> move_left_word
    [] -> zipper
  }
}

pub fn move_right(zipper: Zipper) -> Zipper {
  case zipper.right {
    [first, ..rest] -> Zipper(left: [first, ..zipper.left], right: rest)
    [] -> zipper
  }
}

pub fn insert_grapheme_left(zipper: Zipper, grapheme: String) -> Zipper {
  Zipper(..zipper, left: [grapheme, ..zipper.left])
}

pub fn insert_grapheme_right(zipper: Zipper, grapheme: String) -> Zipper {
  Zipper(..zipper, right: [grapheme, ..zipper.right])
}

pub fn delete_left(zipper: Zipper) -> Zipper {
  case zipper.left {
    [_, ..rest] -> Zipper(..zipper, left: rest)
    [] -> zipper
  }
}

pub fn delete_left_until(zipper: Zipper, grapheme: String) -> Zipper {
  case zipper.left {
    [first, ..rest] if first == grapheme -> Zipper(..zipper, left: rest)
    [_, ..rest] -> Zipper(..zipper, left: rest) |> delete_left_until(grapheme)
    [] -> zipper
  }
}

pub fn delete_left_word(zipper: Zipper) -> Zipper {
  case zipper.left {
    [" ", " ", ..rest] ->
      Zipper(..zipper, left: [" ", ..rest]) |> delete_left_word
    [" ", ..rest] -> Zipper(..zipper, left: rest)
    [_, ..rest] -> Zipper(..zipper, left: rest) |> delete_left_word
    [] -> zipper
  }
}

pub fn delete_right(zipper: Zipper) -> Zipper {
  case zipper.right {
    [_, ..rest] -> Zipper(..zipper, right: rest)
    [] -> zipper
  }
}

pub fn to_string(zipper: Zipper) -> String {
  let left = zipper.left |> list.reverse |> string_tree.from_strings
  let right = zipper.right |> string_tree.from_strings
  string_tree.new()
  |> string_tree.append_tree(left)
  |> string_tree.append_tree(right)
  |> string_tree.to_string
}

//
// WINDOW
//

pub opaque type Window {
  Window(zipper: Zipper, width: Int, right_bias: Int)
}

pub fn to_window(zipper: Zipper, width: Int) -> Window {
  Window(zipper:, width:, right_bias: 0)
}

pub fn move_window_left(window: Window) -> Window {
  case window.zipper.left {
    [first, ..rest] -> {
      let zipper = Zipper(left: rest, right: [first, ..window.zipper.right])
      let right_bias = { window.right_bias + 1 } |> int.min(window.width)
      Window(..window, zipper:, right_bias:)
    }
    [] -> window
  }
}

pub fn move_window_right(window: Window) -> Window {
  case window.zipper.right {
    [first, ..rest] -> {
      let zipper = Zipper(left: [first, ..window.zipper.left], right: rest)
      let right_bias = { window.right_bias - 1 } |> int.max(0)
      Window(..window, zipper:, right_bias:)
    }
    [] -> window
  }
}

pub fn to_windowed_string(window: Window) -> String {
  let left =
    window.zipper.left
    |> list.take(window.width - window.right_bias)
    |> list.reverse
  let left_length = list.length(left)
  let right = window.zipper.right |> list.take(window.width - left_length)
  string_tree.new()
  |> string_tree.append_tree(left |> string_tree.from_strings)
  |> string_tree.append_tree(right |> string_tree.from_strings)
  |> string_tree.to_string
}
