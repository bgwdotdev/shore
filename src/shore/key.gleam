/// note: Many ctrl+key combinations are reserved by either the terminal (e.g.
/// `ctrl+z`, `ctrl+s`) or by the erlang shell (e.g. `ctrl+c`). Make sure to test
/// any ctrl+key combinations being used.
///
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
  Ctrl(String)
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
    "\u{0001}" -> Ctrl("A")
    "\u{0002}" -> Ctrl("B")
    // swallowed by erlang
    "\u{0003}" -> Ctrl("C")
    "\u{0004}" -> Ctrl("D")
    "\u{0005}" -> Ctrl("E")
    "\u{0006}" -> Ctrl("F")
    "\u{0007}" -> Ctrl("G")
    "\u{0008}" -> Ctrl("H")
    // Tab
    //"\u{0009}" -> Ctrl("I")
    // Line Feed (\n)
    //"\u{000A}" -> Ctrl("J")
    "\u{000B}" -> Ctrl("K")
    "\u{000C}" -> Ctrl("L")
    // Carriage Return (\r)
    // "\u{000D}" -> Ctrl("M")
    "\u{000E}" -> Ctrl("N")
    "\u{000F}" -> Ctrl("O")
    "\u{0010}" -> Ctrl("P")
    // Swalloed by erlang?
    "\u{0011}" -> Ctrl("Q")
    "\u{0012}" -> Ctrl("R")
    "\u{0013}" -> Ctrl("S")
    "\u{0014}" -> Ctrl("T")
    "\u{0015}" -> Ctrl("U")
    "\u{0016}" -> Ctrl("V")
    "\u{0017}" -> Ctrl("W")
    "\u{0018}" -> Ctrl("X")
    "\u{0019}" -> Ctrl("Y")
    "\u{001A}" -> Ctrl("Z")

    x -> Char(x)
  }
}
