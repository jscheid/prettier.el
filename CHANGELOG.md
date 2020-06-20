# Changelog

All notable changes to this project will be documented in this file.

The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

Note that "unreleased" changes technically do get released, just not
in a tagged ("stable") version.

## [Unreleased]

## [0.9.0] - 2020-05-27

### Added

- Support for more major modes (via plug-ins): elm, java, pug,
  solidity, svelte, toml, xml.
- Continuous deployment, enables installing via Melpa and Quelpa.

### Changed

- Move Changelog into CHANGELOG.md.
- Dev workflow improvements.
- Documentation improvements.

## [0.8.1] - 2020-05-27

### Fixed

- Fix customization errors (#10).

## [0.8.0] - 2020-05-19

### Added

- Support Yarn Plug'n'Play (#3).
- Add Typescript detection.
- Add support for ANSI escape codes in Prettier error output.
- Setup Continuous Integration.

### Changed

- Drop support for Emacs <26.1.
- Fix linting issues and add some test cases.

### Fixed

- Fix handling of files containing Unicode surrogate pairs.

## [0.7.0] - 2019-08-31

### Changed

- Switch from cl to cl-lib.
- Minor documentation improvements.

## [0.6.3] - 2019-02-24

### Fixed

- Don't choke on buffers not backed by file.

## [0.6.2] - 2019-02-24

### Fixed

- Fix initial and HTML parser detection.

## [0.6.1] - 2019-02-19

### Fixed

- Fix web-mode parser detection for empty buffers.

## [0.6.0] - 2019-02-19

### Added

- Improved parser detection:
  - Use json-stringify for package.json and such.
  - Use babel-flow when flow-minor-mode is active.

## [0.5.1] - 2019-02-19

### Fixed

- Fix web-mode parser detection.

## [0.5.0] - 2019-02-19

### Added

- Compatibility with Prettier 1.16:
  - New parsers.
  - Ignore unsupported parsers.
  - Show parser and version in modeline.
  - Use editorconfig setting when syncing options.

## [0.4.1] - 2018-11-28

### Fixed

- Fix error column with indent-tabs-mode.

## [0.4.0] - 2018-11-28

### Changed

- Improvements for large input files
  - Use diff-match-patch instead of fast-diff, limiting the maximum amount of time spent diffing
  - Handle quit during long-running prettification gracefully
  - Roll back partially applied diffs
  - Don't accumulate diff patches in memory, instead apply them immediately
- Disable undo list in process buffer.

## [0.3.0] - 2018-11-14

### Fixed

- Fix bugs when handling buffers not backed by a file, and buffers for
  which the parser can't be derived from the major mode.
- Don't abstain from auto-enabling prettier-mode in buffers not backed
  by a file, as long as the major mode is supported.

## [0.2.3] - 2018-11-13

### fixed

- Fix error in buffers without file name.

## [0.2.2] - 2018-11-13

### Changed

- When point is point-max, keep it there.

## [0.2.1] - 2018-11-10

### Added

- Reinstate web-mode parser detection.

### Fixed

- Ignore NaN result cursorOffset.

## [0.2.0] - 2018-11-10

### Added

- Allow ignoring certain files for prettier-mode.

### Changed

- Temporarily use locally built pandoc, improves info formatting.

### Fixed

- Remove dead code that was causing a crash sometimes.

## [0.1.2] - 2018-11-08

## [0.1.1] - 2018-11-08

## [0.1.0] - 2018-10-12

[unreleased]: https://github.com/jscheid/prettier.el/compare/v0.9.0...HEAD
[0.9.0]: https://github.com/jscheid/prettier.el/compare/v0.8.1...v0.9.0
[0.8.1]: https://github.com/jscheid/prettier.el/compare/v0.8.0...v0.8.1
[0.8.0]: https://github.com/jscheid/prettier.el/compare/v0.7.0...v0.8.0
[0.7.0]: https://github.com/jscheid/prettier.el/compare/v0.6.3...v0.7.0
[0.6.3]: https://github.com/jscheid/prettier.el/compare/v0.6.2...v0.6.3
[0.6.2]: https://github.com/jscheid/prettier.el/compare/v0.6.1...v0.6.2
[0.6.1]: https://github.com/jscheid/prettier.el/compare/v0.6.0...v0.6.1
[0.6.0]: https://github.com/jscheid/prettier.el/compare/v0.5.1...v0.6.0
[0.5.1]: https://github.com/jscheid/prettier.el/compare/v0.5.0...v0.5.1
[0.5.0]: https://github.com/jscheid/prettier.el/compare/v0.4.1...v0.5.0
[0.4.1]: https://github.com/jscheid/prettier.el/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/jscheid/prettier.el/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/jscheid/prettier.el/compare/v0.2.3...v0.3.0
[0.2.3]: https://github.com/jscheid/prettier.el/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/jscheid/prettier.el/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/jscheid/prettier.el/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/jscheid/prettier.el/compare/v0.1.5...v0.2.0
[0.1.5]: https://github.com/jscheid/prettier.el/compare/v0.1.4...v0.1.5
[0.1.4]: https://github.com/jscheid/prettier.el/compare/v0.1.3...v0.1.4
[0.1.3]: https://github.com/jscheid/prettier.el/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/jscheid/prettier.el/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/jscheid/prettier.el/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/jscheid/prettier.el/releases/tag/v0.1.0
