/// Used to set all children of `Aligned` to the selected alignment.
///
pub type Align {
  Left
  Center
  Right
}

/// Configurable size options/units for ui elements
pub type Size {
  /// Absolute width/height
  Px(Int)
  /// A percentage of the parent item width/height
  Pct(Int)
  /// Will fit the item to the parents width/height. When multiple exist within
  /// a single item, such as a layout, will automatically divide the space
  /// between items.
  Fill
}

/// ANSI terminal color. Used for setting the background/foreground of UI
/// elements.
///
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
