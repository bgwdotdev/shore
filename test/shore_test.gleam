import gleeunit
import shore/internal/zipper

pub fn main() {
  gleeunit.main()
}

const fox = "the quick brown fox"

//
// ZIPPER
//

pub fn zipper_init_test() {
  let actual =
    fox
    |> zipper.from_string
    |> zipper.to_string
  let expect = "the quick brown fox"
  assert expect == actual
}

pub fn zipper_insert_test() {
  let actual =
    fox
    |> zipper.from_string
    |> zipper.insert_grapheme_left("!")
    |> zipper.to_string
  let expect = "the quick brown fox!"
  assert expect == actual
}

pub fn zipper_delete_test() {
  let actual =
    fox
    |> zipper.from_string
    |> zipper.delete_left
    |> zipper.to_string
  let expect = "the quick brown fo"
  assert expect == actual
}

pub fn zipper_move_delete_insert_test() {
  let actual =
    fox
    |> zipper.from_string
    |> repeat(zipper.move_left, 3)
    |> zipper.insert_grapheme_left("w")
    |> zipper.insert_grapheme_left("e")
    |> zipper.insert_grapheme_left("e")
    |> zipper.insert_grapheme_left(" ")
    |> zipper.to_string
  let expect = "the quick brown wee fox"
  assert expect == actual
}

pub fn zipper_move_word_test() {
  let actual =
    fox
    |> zipper.from_string
    |> zipper.move_left_word
    |> zipper.insert_grapheme_left(" ")
    |> zipper.insert_grapheme_left("w")
    |> zipper.insert_grapheme_left("e")
    |> zipper.insert_grapheme_left("e")
    |> zipper.to_string
  let expect = "the quick brown wee fox"
  assert expect == actual
}

pub fn zipper_delete_word_test() {
  let actual =
    "the quick brown    fox"
    |> zipper.from_string
    |> zipper.delete_left_word
    |> zipper.to_string
  let expect = "the quick brown"
  assert expect == actual
}

//
// WINDOW
//

pub fn window_init_test() {
  let actual =
    fox
    |> zipper.from_string
    |> zipper.to_window(5)
    |> zipper.to_windowed_string
  let expect = "n fox"
  assert expect == actual
}

pub fn window_move_left_test() {
  let actual =
    fox
    |> zipper.from_string
    |> zipper.to_window(5)
    |> repeat(zipper.move_window_left, 6)
    |> zipper.to_windowed_string
  let expect = "wn fo"
  assert expect == actual
}

pub fn window_move_left_right_test() {
  let actual =
    fox
    |> zipper.from_string
    |> zipper.to_window(5)
    |> repeat(zipper.move_window_left, 6)
    |> repeat(zipper.move_window_right, 6)
    |> zipper.to_windowed_string
  let expect = "n fox"
  assert expect == actual
}

//
// HELPERS
//

fn repeat(a: a, fun: fn(a) -> a, count: Int) -> a {
  case count {
    0 -> a
    _ -> fun(a) |> repeat(fun, count - 1)
  }
}
