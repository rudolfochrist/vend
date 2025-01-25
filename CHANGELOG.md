# vend

## Unreleased

#### Added

- New command: `vend test`. Supports:
  - `parachute`
  - `clunit2`
  - `fiveam`

#### Fixed

- Updated `transducers` dependency to fix a `.asd` detection edge case.

## 0.1.3 (2025-01-19)

#### Added

- New command: `vend search`.
- Support for Allegro (`alisp`).
- Numerous additions to the registry.

#### Fixed

- Restored support for `postmodern`.

## 0.1.2 (2025-01-17)

#### Added

- `vend check` also warns about dependencies that couldn't be fetched.
- Improved dependency scanning accuracy and performance.

## 0.1.1 (2025-01-15)

#### Added

- Dependency support for more large projects.

#### Fixed

- An odd additional message when dependency resolution fails.
- Ignore `.qlot/` if present.

## 0.1.0 (2025-01-13)

Initial release.
