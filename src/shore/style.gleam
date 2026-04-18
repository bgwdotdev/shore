/// Used to set all children of `Aligned` to the selected alignment.
///
pub type Align {
  Left
  Center
  Right
}

/// Configurable size options/units for ui elements
pub type Size {
  /// Absolute width/height, a.k.a terminal columns/rows.
  Px(Int)
  /// A percentage of the parent item width/height
  Pct(Int)
  /// Will fit the item to the parents width/height. When multiple exist within
  /// a single item, such as a layout, will automatically divide the space
  /// between items.
  Fill
  /// A minimum size and a maximum size
  ///
  /// ## Example
  ///
  /// In the scenario of setting a width on a box:
  ///
  /// ```
  /// // 80% of the screen, up to 100 characters wide
  /// style.MinMax(style.Pct(80), style.Px(100))
  ///
  /// // 80% of the screen, but no less than 20 characters wide
  /// style.MinMax(style.Px(20), style.Pct(80))
  ///
  /// // 20 characters wide if the parent width is less than 100 characters
  /// // 100 characters wide otherwise
  /// style.MinMax(style.Px(20), style.Px(100))
  /// ```
  ///
  MinMax(min: Size, max: Size)
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

pub type Graphic {
  Bold
  Faint
  Italic
  Underline
  Inverse
  Conceal
  Strikethrough
}
