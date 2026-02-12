# copl-rs

`copl-rs` is a Rust project for implementing CoPL derivation checkers.
The current target is a checker for the `Nat` game.

## Current Status

- Implementation phase: M3 (Nat checker validation)
- Implemented:
  - CLI: `copl-rs checker --game nat [file]`
  - `stdin` input when `[file]` is omitted
  - Game dispatch via `enum GameKind + match`
  - Input size limit (8 MiB) and UTF-8 validation
  - Nat ASCII parser (`judgment + raw rule name + subderivations`)
  - Nat derivation rule validation (`P-Zero`, `P-Succ`, `T-Zero`, `T-Succ`)
- Not implemented yet:
  - `resolver` subcommand
  - Machine-readable (JSON) error output mode

## Requirements

- Rust (`cargo`, `rustc`)
- Git (for submodules)

If Rust is not installed yet, use the official installer:

```sh
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

## Setup

```sh
git clone <this-repo>
cd copl-rs
git submodule update --init --recursive
```

`copl/` is tracked as a submodule:
`git@github.com:wat-aro/copl.git`

## Usage

### File Input

```sh
cargo run -- checker --game nat copl/001.copl
```

### Stdin Input

```sh
cat copl/001.copl | cargo run -- checker --game nat
```

### File Names Starting With `-`

Use `--` before positional arguments:

```sh
cargo run -- checker --game nat -- -input.copl
```

## Tests

```sh
cargo test
cargo clippy --all-targets --all-features -- -D warnings
```

## Directory Layout

```text
src/
  main.rs          # CLI entry point
  lib.rs           # Application flow
  cli.rs           # Argument parsing
  core/            # Shared types (GameKind, Game trait, Error/Report)
  games/
    mod.rs         # Game dispatch
    nat/
      mod.rs
      syntax.rs
      parser.rs
      checker.rs
docs/PLAN.md       # Implementation plan and progress
AGENTS.md          # Agreed project policies
```

## Development Process

- Follow t-wada style TDD (`Red -> Green -> Refactor`)
- Keep progress updated in `docs/PLAN.md`
