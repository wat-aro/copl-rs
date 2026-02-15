# copl-rs

`copl-rs` is a Rust project for implementing CoPL derivation checkers.
The current targets are checkers for the `Nat`, `CompareNat1`, `CompareNat2`, `CompareNat3`, `EvalML1`, `EvalML1Err`, `EvalML2`, `EvalML3`, `EvalML4`, `EvalML5`, `TypingML4`, `PolyTypingML4`, `NamelessML3`, `EvalNamelessML3`, `EvalNatExp`, and `ReduceNatExp` games.

## Current Status

- Implementation priorities are managed in the unified backlog in `docs/PLAN.md`.
- Implemented:
  - CLI: `copl-rs checker --game <name> [file]`
  - Supported `--game` values: `Nat`, `CompareNat1`, `CompareNat2`, `CompareNat3`, `EvalML1`, `EvalML1Err`, `EvalML2`, `EvalML3`, `EvalML4`, `EvalML5`, `TypingML4`, `PolyTypingML4`, `NamelessML3`, `EvalNamelessML3`, `EvalNatExp`, `ReduceNatExp` (lowercase forms are also accepted for backward compatibility)
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
  - EvalML1 ASCII parser (`judgment + raw rule name + subderivations`)
  - EvalML1 derivation rule validation (`E-*`, `B-*`)
  - EvalML1Err ASCII parser (`judgment + raw rule name + subderivations`)
  - EvalML1Err derivation rule validation (`E-*`, `B-*`)
  - EvalML2 ASCII parser (`judgment + raw rule name + subderivations`)
  - EvalML2 derivation rule validation (`E-*`, `B-*`)
  - EvalML3 ASCII parser (`judgment + raw rule name + subderivations`)
  - EvalML3 derivation rule validation (`E-*`, `B-*`, including `let rec`, `fun`, and application)
  - EvalML4 ASCII parser (`judgment + raw rule name + subderivations`)
  - EvalML4 derivation rule validation (`E-*`, `B-*`, including lists (`[]`, `::`) and pattern matching)
  - EvalML5 ASCII parser (`judgment + raw rule name + subderivations`)
  - EvalML5 derivation rule validation (`E-*`, `M-*`, `NM-*`, `B-*`, including multi-clause pattern matching)
  - TypingML4 ASCII parser (`judgment + raw rule name + subderivations`)
  - TypingML4 derivation rule validation (`T-*`, including function/list types and list pattern matching)
  - PolyTypingML4 ASCII parser (`judgment + raw rule name + subderivations`)
  - PolyTypingML4 derivation rule validation (`T-*`, including polymorphic type schemes (`'a ... . t`), instantiation, and let-generalization)
  - NamelessML3 ASCII parser (`judgment + raw rule name + subderivations`)
  - NamelessML3 derivation rule validation (`Tr-*` for named-to-nameless translation)
  - EvalNamelessML3 ASCII parser (`judgment + raw rule name + subderivations`)
  - EvalNamelessML3 derivation rule validation (`E-*`, `B-*`, including de Bruijn-indexed `E-Var` and recursive application)
  - EvalNatExp ASCII parser (`judgment + raw rule name + subderivations`)
  - EvalNatExp derivation rule validation (`E-Const`, `E-Plus`, `E-Times`, `P-Zero`, `P-Succ`, `T-Zero`, `T-Succ`)
  - ReduceNatExp ASCII parser (`judgment + raw rule name + subderivations`)
  - ReduceNatExp derivation rule validation (`R-*`, `DR-*`, `MR-*`, `P-*`, `T-*`)
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
cargo run -- checker --game EvalML1 copl/025.copl
```

```sh
cargo run -- checker --game EvalML1Err copl/031.copl
```

```sh
cargo run -- checker --game EvalML2 copl/034.copl
```

```sh
cargo run -- checker --game EvalML3 copl/040.copl
```

```sh
cargo run -- checker --game EvalML4 copl/070.copl
```

```sh
cargo run -- checker --game EvalML5 copl/078.copl
```

```sh
cargo run -- checker --game TypingML4 copl/080.copl
```

```sh
cargo run -- checker --game PolyTypingML4 copl/107.copl
```

```sh
cargo run -- checker --game NamelessML3 copl/054.copl
```

```sh
cargo run -- checker --game EvalNamelessML3 copl/055.copl
```

```sh
cargo run -- checker --game EvalNatExp copl/015.copl
```

```sh
cargo run -- checker --game ReduceNatExp copl/021.copl
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
    eval_ml1/
      mod.rs
      syntax.rs
      parser.rs
      checker.rs
    eval_ml1_err/
      mod.rs
      syntax.rs
      parser.rs
      checker.rs
    eval_ml2/
      mod.rs
      syntax.rs
      parser.rs
      checker.rs
    eval_ml3/
      mod.rs
      syntax.rs
      lexer.rs
      parser.rs
      checker.rs
    eval_ml4/
      mod.rs
      syntax.rs
      lexer.rs
      parser.rs
      checker.rs
    eval_ml5/
      mod.rs
      syntax.rs
      lexer.rs
      parser.rs
      checker.rs
    typing_ml4/
      mod.rs
      syntax.rs
      lexer.rs
      parser.rs
      checker.rs
    poly_typing_ml4/
      mod.rs
      syntax.rs
      lexer.rs
      parser.rs
      checker.rs
    nameless_ml3/
      mod.rs
      syntax.rs
      lexer.rs
      parser.rs
      checker.rs
    eval_nameless_ml3/
      mod.rs
      syntax.rs
      lexer.rs
      parser.rs
      checker.rs
    reduce_nat_exp/
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
