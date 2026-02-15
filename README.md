# copl-rs

`copl-rs` is a Rust project for implementing CoPL derivation checkers.
The current targets are checkers for the `Nat`, `CompareNat1`, `CompareNat2`, `CompareNat3`, and `EvalNatExp` games.

## Current Status

- Implementation priorities are managed in the unified backlog in `docs/PLAN.md`.
- Implemented:
  - CLI: `copl-rs checker --game <name> [file]`
  - Supported `--game` values: `Nat`, `CompareNat1`, `CompareNat2`, `CompareNat3`, `EvalNatExp` (lowercase forms are also accepted for backward compatibility)
  - `stdin` input when `[file]` is omitted
  - Game dispatch via `enum GameKind + match`
  - Input size limit (8 MiB) and UTF-8 validation
  - Nat ASCII parser (`judgment + raw rule name + subderivations`)
  - Nat derivation rule validation (`P-Zero`, `P-Succ`, `T-Zero`, `T-Succ`)
  - CompareNat1 ASCII parser (`judgment + raw rule name + subderivations`)
  - CompareNat1 derivation rule validation (`L-Succ`, `L-Trans`)
  - CompareNat2 ASCII parser (`judgment + raw rule name + subderivations`)
  - CompareNat2 derivation rule validation (`L-Zero`, `L-SuccSucc`)
  - CompareNat3 ASCII parser (`judgment + raw rule name + subderivations`)
  - CompareNat3 derivation rule validation (`L-Succ`, `L-SuccR`)
  - EvalNatExp ASCII parser (`judgment + raw rule name + subderivations`)
  - EvalNatExp derivation rule validation (`E-Const`, `E-Plus`, `E-Times`, `P-Zero`, `P-Succ`, `T-Zero`, `T-Succ`)
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

```sh
cargo run -- checker --game CompareNat2 copl/010.copl
```

```sh
cargo run -- checker --game CompareNat3 copl/011.copl
```

```sh
cargo run -- checker --game EvalNatExp copl/015.copl
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
    compare_nat2/
      mod.rs
      syntax.rs
      parser.rs
      checker.rs
    compare_nat3/
      mod.rs
      syntax.rs
      parser.rs
      checker.rs
    eval_nat_exp/
      mod.rs
      syntax.rs
      parser.rs
      checker.rs
    nat/
      mod.rs
      syntax.rs
      parser.rs
      checker.rs
docs/PLAN.md       # Current prioritized plan
docs/plans/        # Archived plan history
docs/how-to-add-a-game.md  # Steps to add a new game module
AGENTS.md          # Agreed project policies
```

## Development Process

- Follow t-wada style TDD (`Red -> Green -> Refactor`)
- Keep progress updated in `docs/PLAN.md`
- Archive completed or frozen plans under `docs/plans/`
