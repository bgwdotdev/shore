import gleam/list

//
// EFFECTS
//
pub opaque type Effect(msg) {
  Effect(effects: List(fn(fn(msg) -> Nil) -> Nil))
}

pub fn new(callback: fn() -> msg) -> Effect(msg) {
  [callback] |> from_list
}

pub fn from_list(callbacks: List(fn() -> msg)) -> Effect(msg) {
  list.map(callbacks, fn(callback) { fn(do) { callback() |> do } }) |> Effect
}

//pub fn from_promise(callback: fn() -> Promise(msg)) -> Effect(msg) {
//  Effect([
//    fn(do) {
//      callback() |> promise.tap(do)
//      Nil
//    },
//  ])
//}

pub fn none() -> Effect(msg) {
  Effect([])
}

@internal
pub fn to_list(effect: Effect(msg)) -> List(fn(fn(msg) -> Nil) -> Nil) {
  effect.effects
}
