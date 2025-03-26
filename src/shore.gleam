import gleam/io

pub fn main() {
  io.println("Hello from shore!")
  echo hello_nif()
}

@external(erlang, "shore_ffi", "hello_nif")
fn hello_nif() -> Int

@external(erlang, "shore_ffi", "init")
fn init() -> Int
