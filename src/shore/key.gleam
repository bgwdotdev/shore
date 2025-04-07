pub type Key {
  Backspace
  Enter
  Left
  Right
  Up
  Down
  Home
  End
  PageUp
  PageDown
  Tab
  BackTab
  Delete
  Insert
  F(Int)
  Char(String)
  Null
  Esc
  //CapsLock
  //ScrollLock
  //NumLock
  //PrintScreen
  //Pause
  //Menu
  //KeypadBegin
  //Media(MediaKeyCode)
  //Modifier(ModifierKeyCode)
}

pub fn from_string(key: String) -> Key {
  case key {
    "\u{0007F}" -> Backspace
    "\r" -> Enter
    "\u{001B}[D" -> Left
    "\u{001B}[C" -> Right
    "\u{001B}[A" -> Up
    "\u{001B}[B" -> Down
    "\u{001B}[H" -> Home
    "\u{001B}[F" -> End
    "\u{001B}[5~" -> PageUp
    "\u{001B}[6~" -> PageDown
    "\t" -> Tab
    "\u{001B}[Z" -> BackTab
    "\u{001B}[3~" -> Delete
    "\u{001B}[2~" -> Insert
    "\u{001B}" -> Esc
    "" -> Null
    "\u{001B}OP" -> F(1)
    "\u{001B}OQ" -> F(2)
    "\u{001B}OR" -> F(3)
    "\u{001B}OS" -> F(4)
    "\u{001B}[15~" -> F(5)
    "\u{001B}[17~" -> F(6)
    "\u{001B}[18~" -> F(7)
    "\u{001B}[19~" -> F(8)
    "\u{001B}[20~" -> F(9)
    "\u{001B}[21~" -> F(10)
    "\u{001B}[23~" -> F(11)
    "\u{001B}[24~" -> F(12)
    x -> Char(x)
  }
}
