# copl-rs

`copl-rs` is a Rust project for implementing CoPL derivation checkers.
The current targets are checkers for the `Nat` and `CompareNat1` games.

## Current Status

- Implementation phase: M4 (extension path documentation)
- Implemented:
  - CLI: `copl-rs checker --game <name> [file]`
  - Supported `--game` values: `Nat`, `CompareNat1` (lowercase forms are also accepted for backward compatibility)
  - `stdin` input when `[file]` is omitted
  - Game dispatch via `enum GameKind + match`
  - Input size limit (8 MiB) and UTF-8 validation
  - Nat ASCII parser (`judgment + raw rule name + subderivations`)
  - Nat derivation rule validation (`P-Zero`, `P-Succ`, `T-Zero`, `T-Succ`)
  - CompareNat1 ASCII parser (`judgment + raw rule name + subderivations`)
  - CompareNat1 derivation rule validation (`L-Succ`, `L-Trans`)
  - Successful checks print inferred root judgment text (reference-implementation-compatible)
  - Rule-violation diagnostics with source location (`line:column`)
  - Rule-violation diagnostics include actionable hints (`expected` / `actual` / `fix`) when available
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
cargo run -- checker --game Nat copl/001.copl
```

```sh
cargo run -- checker --game CompareNat1 copl/009.copl
```

### Stdin Input

```sh
cat copl/001.copl | cargo run -- checker --game Nat
```

Expected output format (success):

```text
Z plus Z is Z
```

### File Names Starting With `-`

Use `--` before positional arguments:

```sh
cargo run -- checker --game Nat -- -input.copl
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
    compare_nat1/
      mod.rs
      syntax.rs
      parser.rs
      checker.rs
    nat/
      mod.rs
      syntax.rs
      parser.rs
      checker.rs
docs/PLAN.md       # Plan index (active plan + history links)
docs/plans/        # Named plan documents
docs/how-to-add-a-game.md  # Steps to add a new game module
AGENTS.md          # Agreed project policies
```

## Development Process

- Follow t-wada style TDD (`Red -> Green -> Refactor`)
- Keep progress updated in `docs/PLAN.md` and the active file under `docs/plans/`
