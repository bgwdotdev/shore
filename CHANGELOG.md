# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- ui: add `box_styled` function which allows customising the border colour of a box

### Changed

- render: change render logic to only send redraw calls if the previous frame
  is different from the next, this largely reduces flickering in tmux though is
  still mostly a band-aid until proper diff-based rendering logic is
  implemented.

### Fixed

- ui: fix issue with focusing when using `align`
- layout: fix issue where focusable items in a grid were walked in reverse

## [1.2.0] - 2025-07-31

### Added

- backend: add support for custom renderers to support `beach` companion
  project which allows serving shore apps over ssh
- spec: add `spec_with_subject` function which provides support for passing an
  shore `msg` subject to the init function for use inside shore applications
- ui: add `input_submit` function which allows having a 'submit' event attached
  to an input field which will trigger when the `submit` keybind is pressed
  while the input field is focused
- ui: add `button_styled` function which allows customising button colours
- ui: add `text_wrapped` functions which provide automatic text wrapping
- ui: add `image_unstable` function which allows using the kitty graphics
  protocol to display images, note that this is an unstable feature and will
  likely be reworked and/or removed at a later date

### Fixed

- draw: fix ui drawing issue when using on_update with focusable items
- input: fix input field cursor navigation
- input: fix paste support for input

## [1.1.0] - 2025-06-20

### Added

- ui: add `input_hidden` node for usage such as password fields

### Changed

- dep: update `gleam_otp` from `0.16.1` to `1.0.0`
- dep: update `gleam_erlang` from `0.34.0` to `1.0.0`
