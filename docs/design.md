# copl-rs Design (Snapshot)

Last updated: 2026-02-19

## 1. Purpose

This document captures the current architecture and design decisions for `copl-rs`.
It is a snapshot of the implementation state and the agreed extension direction.

## 2. Scope

### In scope now

- Single-crate Rust project.
- CLI entry point as `copl-rs`.
- `checker` subcommand with unified game selection:
  - `copl-rs checker --game <name> [file]`
- `prover` subcommand CLI parsing and runtime route:
  - `copl-rs prover --game <name> [file]`
- Current game targets: `Nat`, `CompareNat1`, `CompareNat2`, `CompareNat3`, `EvalML1`, `EvalML1Err`, `EvalML2`, `EvalML3`, `EvalML4`, `EvalML5`, `EvalContML1`, `EvalContML4`, `TypingML4`, `PolyTypingML4`, `NamelessML3`, `EvalNamelessML3`, `EvalNatExp`, `ReduceNatExp`.

### Out of scope now

- Prover implementation for games other than `Nat`, `CompareNat1`, `CompareNat2`, `CompareNat3`, `EvalML1`, `EvalML1Err`, `EvalML2`, `EvalML3`, `EvalML4`, and `EvalML5`.
- JSON/machine-readable error output format.

## 3. Architecture Overview

The project is split into explicit module boundaries:

- `src/main.rs`:
  - Thin executable entry.
  - Calls `copl_rs::run(...)`.
- `src/lib.rs`:
  - Application orchestration.
  - Input reading, UTF-8 validation, size limits, checker execution.
- `src/cli.rs`:
  - CLI domain model and subcommand dispatch.
- `src/cli/checker.rs`:
  - `checker` subcommand parser adapter.
- `src/cli/prover.rs`:
  - `prover` subcommand parser adapter.
- `src/cli/game_command.rs`:
  - Shared typed-state parser for `--game <name> [file]` / `stdin` / `--` contract.
- `src/core/mod.rs`:
  - Shared domain contracts (`GameKind`, `Game`, `CheckReport`, `CheckError`).
- `src/games/mod.rs`:
  - Game registry/dispatch via `enum GameKind + match`.
- `src/games/nat_arith.rs`:
  - Shared structural validators for Nat arithmetic rules (`P-*`, `T-*`) used by `Nat`, `EvalNatExp`, and `ReduceNatExp` checkers.
- `src/games/compare_nat1/`:
  - `syntax.rs`, `parser.rs`, `checker.rs`, `prover.rs`, `mod.rs` (`prove`).
- `src/games/compare_nat2/`:
  - `syntax.rs`, `parser.rs`, `checker.rs`, `prover.rs`, `mod.rs` (`prove`).
- `src/games/compare_nat3/`:
  - `syntax.rs`, `parser.rs`, `checker.rs`, `prover.rs`, `mod.rs` (`prove`).
- `src/games/eval_ml1/`:
  - `syntax.rs`, `parser.rs`, `checker.rs`, `prover.rs`, `mod.rs` (`prove`).
- `src/games/eval_ml1_err/`:
  - `syntax.rs`, `parser.rs`, `checker.rs`, `prover.rs`, `mod.rs` (`prove`).
- `src/games/eval_ml2/`:
  - `syntax.rs`, `parser.rs`, `checker.rs`, `prover.rs`, `mod.rs` (`prove`).
- `src/games/eval_ml3/`:
  - `syntax.rs`, `lexer.rs`, `parser.rs`, `checker.rs`, `prover.rs`, `mod.rs` (`prove`).
- `src/games/eval_ml4/`:
  - `syntax.rs`, `lexer.rs`, `parser.rs`, `checker.rs`, `prover.rs`, `mod.rs` (`prove`).
- `src/games/eval_ml5/`:
  - `syntax.rs`, `lexer.rs`, `parser.rs`, `checker.rs`, `prover.rs`, `mod.rs` (`prove`).
- `src/games/eval_cont_ml1/`:
  - `syntax.rs`, `lexer.rs`, `parser.rs`, `checker.rs`.
- `src/games/eval_cont_ml4/`:
  - `syntax.rs`, `lexer.rs`, `parser.rs`, `checker.rs`.
- `src/games/typing_ml4/`:
  - `syntax.rs`, `lexer.rs`, `parser.rs`, `checker.rs`.
- `src/games/poly_typing_ml4/`:
  - `syntax.rs`, `lexer.rs`, `parser.rs`, `checker.rs`.
- `src/games/nameless_ml3/`:
  - `syntax.rs`, `lexer.rs`, `parser.rs`, `checker.rs`.
- `src/games/eval_nameless_ml3/`:
  - `syntax.rs`, `lexer.rs`, `parser.rs`, `checker.rs`.
- `src/games/eval_nat_exp/`:
  - `syntax.rs`, `parser.rs`, `checker.rs`.
- `src/games/reduce_nat_exp/`:
  - `syntax.rs`, `parser.rs`, `checker.rs`.
- `src/games/nat/`:
  - `syntax.rs`, `parser.rs`, `checker.rs`, `prover.rs`, `mod.rs` (`prove`).

This structure is intended to localize game-specific logic under `games/<game>/`.

## 4. CLI Design

### 4.1 Command model

`Cli` uses a typed command tree:

- `Cli { command: Command }`
- `Command::Checker(CheckerCommand)`
- `Command::Prover(ProverCommand)`
- `CheckerCommand { game: GameKind, input: InputSource }`
- `ProverCommand { game: GameKind, input: InputSource }`
- `InputSource::Stdin | InputSource::File(PathBuf)`

### 4.2 Parsing approach

CLI parsing is split by subcommand module.

- `Cli::parse` resolves the subcommand name and delegates to the corresponding parser module.
- Game/input parsing is implemented in `src/cli/game_command.rs` as a typed state machine and reused by `checker` / `prover` adapters:
  - Internal parser state uses typestate-like markers:
    - mode-like markers: `ModeOptions`, `ModePositional`
    - expectation markers: `ExpectAny`, `ExpectGameValue`
  - Token classification in option mode is separated by `classify_options_event(...)`.
  - Input processing uses `try_fold` over argument tokens.

This avoids index-mutation style loops and keeps transitions explicit.

### 4.3 Compatibility guarantees

Current compatibility rules:

- Keep subcommand contracts stable:
  - `copl-rs checker --game <name> [file]`
  - `copl-rs prover --game <name> [file]`
- If `[file]` is omitted, read from `stdin`.
- Support `--` to treat following `-`-prefixed tokens as positional file names.
- Keep game-name parsing case-insensitive.

## 5. Runtime Flow

`main -> lib::run -> cli::parse -> execute -> {checker route | prover route}`

Key runtime checks in `lib.rs`:

- Input source selection (`stdin` or file).
- Maximum input size:
  - test build: 1024 bytes
  - non-test build: 8 MiB
- UTF-8 validation.
- `prover --game Nat` validates judgment-only input syntax before proving.
- `prover --game CompareNat1` validates judgment-only input syntax before proving.
- `prover --game CompareNat2` validates judgment-only input syntax before proving.
- `prover --game CompareNat3` validates judgment-only input syntax before proving.
- `prover --game EvalML1` validates judgment-only input syntax before proving.
- `prover --game EvalML1Err` validates judgment-only input syntax before proving.
- `prover --game EvalML2` validates judgment-only input syntax before proving.
- `prover --game EvalML3` validates judgment-only input syntax before proving.
- `prover --game EvalML4` validates judgment-only input syntax before proving.
- `prover --game EvalML5` validates judgment-only input syntax before proving.
- Nat prover core builds an in-memory derivation AST using `P-Zero` / `P-Succ` / `T-Zero` / `T-Succ`.
- CompareNat1 prover core builds an in-memory derivation AST using `L-Succ` / `L-Trans`.
- CompareNat2 prover core builds an in-memory derivation AST using `L-Zero` / `L-SuccSucc`.
- CompareNat3 prover core builds an in-memory derivation AST using `L-Succ` / `L-SuccR`.
- EvalML1 prover core builds an in-memory derivation AST using `E-*` / `B-*` rules in a deterministic evaluation order.
- EvalML1Err prover core builds an in-memory derivation AST using `E-*` / `B-*` rules in a deterministic evaluation order, including type-error and error-propagation branches.
- EvalML2 prover core builds an in-memory derivation AST using `E-*` / `B-*` rules in a deterministic evaluation order, including environment-sensitive rules (`E-Var1`, `E-Var2`, `E-Let`).
- EvalML3 prover core builds an in-memory derivation AST using `E-*` / `B-*` rules in a deterministic evaluation order, including environment-sensitive rules (`E-Var1`, `E-Var2`, `E-LetRec`, `E-AppRec`).
- EvalML4 prover core builds an in-memory derivation AST using `E-*` / `B-*` rules in a deterministic evaluation order, including list/match rules (`E-Nil`, `E-Cons`, `E-MatchNil`, `E-MatchCons`) and nearest-binding variable lookup (`E-Var`).
- EvalML5 prover core builds an in-memory derivation AST using `E-*` / `M-*` / `NM-*` / `B-*` rules in a deterministic evaluation order, including multi-clause pattern matching (`E-MatchM1`, `E-MatchM2`, `E-MatchN`) and pattern-relation judgments.
- Nat/CompareNat1/CompareNat2/CompareNat3/EvalML1/EvalML1Err/EvalML2/EvalML3/EvalML4/EvalML5 provers render derivation trees in checker-compatible plain text and write them to stdout.
- `prover` for games other than `Nat` / `CompareNat1` / `CompareNat2` / `CompareNat3` / `EvalML1` / `EvalML1Err` / `EvalML2` / `EvalML3` / `EvalML4` / `EvalML5` returns `RunError::ProverNotImplemented`.

## 6. Error Model

- CLI parse errors are represented by `CliError`.
- Checker-level errors use `core::CheckError` with:
  - `CheckErrorKind` (`Parse`, `RuleViolation`, `Internal`)
  - optional `SourceSpan`.
- Top-level runtime errors are wrapped in `RunError`.
- For non-derivable Nat, CompareNat1, CompareNat2, CompareNat3, EvalML1, EvalML1Err, EvalML2, EvalML3, EvalML4, and EvalML5 prover judgments, diagnostics are plain-text `RuleViolation` messages with `expected` / `actual` / `fix` where available.

Error output is currently plain text.
On successful check, output is the inferred root judgment text in plain text (ADR-0008).

## 7. Game Implementation Status

Current Nat checker validates derivation trees parsed from CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`).
- `syntax.rs` models Nat judgments as an enum with explicit forms (`PlusIs`, `TimesIs`) (ADR-0007).
- `NatTerm` / `NatJudgment` rendering for CLI output and diagnostics is defined in `syntax.rs` (`Display` impls).
- `NatDerivation` rendering in checker-compatible textual form is defined in `syntax.rs` and used by `prover --game Nat`.
- Nat prover reports non-derivable judgments with actionable plain-text diagnostics (`expected` / `actual` / `fix`), including the corrected result term when derivable.
- `checker.rs` validates Nat rule constraints (`P-Zero`, `P-Succ`, `T-Zero`, `T-Succ`).
- Rule definitions are fixed and referenced statically from checker-local rule IDs.
- Rule application checks are written as per-rule pattern matching over `subderivations`, aligned with the reference checker style (ADR-0007).
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`) and failing premise path (`root`, `1`, `1.2`, ...).
- `RuleViolation` diagnostics include actionable hints (`expected` / `actual` / `fix`) where available.
- Successful check result text is the inferred root judgment (`... plus ... is ...` / `... times ... is ...`) (ADR-0008).

Current CompareNat1 checker/prover validates or generates derivation trees for CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`).
- `parser.rs` also parses judgment-only prover input (`... is less than ...`).
- `syntax.rs` models CompareNat1 judgments in `left is less than right` form.
- `syntax.rs` includes `CompareNat1Derivation` rendering in checker-compatible textual form.
- `checker.rs` validates CompareNat1 rule constraints (`L-Succ`, `L-Trans`).
- `prover.rs` deterministically constructs CompareNat1 derivations by chaining `L-Succ` and `L-Trans`.
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`... is less than ...`).
- Non-derivable prover judgments are reported as plain-text `RuleViolation` with actionable hints (`expected` / `actual` / `fix`), including the corrected right-hand term.

Current CompareNat2 checker/prover validates or generates derivation trees for CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`).
- `parser.rs` also parses judgment-only prover input (`... is less than ...`).
- `syntax.rs` models CompareNat2 judgments in `left is less than right` form.
- `syntax.rs` includes `CompareNat2Derivation` rendering in checker-compatible textual form.
- `checker.rs` validates CompareNat2 rule constraints (`L-Zero`, `L-SuccSucc`).
- `prover.rs` deterministically constructs CompareNat2 derivations by recursively applying `L-SuccSucc` and closing with `L-Zero`.
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`... is less than ...`).
- Non-derivable prover judgments are reported as plain-text `RuleViolation` with actionable hints (`expected` / `actual` / `fix`), including the corrected right-hand term.

Current CompareNat3 checker/prover validates or generates derivation trees for CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`).
- `parser.rs` also parses judgment-only prover input (`... is less than ...`).
- `syntax.rs` models CompareNat3 judgments in `left is less than right` form.
- `syntax.rs` includes `CompareNat3Derivation` rendering in checker-compatible textual form.
- `checker.rs` validates CompareNat3 rule constraints (`L-Succ`, `L-SuccR`).
- `prover.rs` deterministically constructs CompareNat3 derivations by recursively applying `L-SuccR` and closing with `L-Succ`.
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`... is less than ...`).
- Non-derivable prover judgments are reported as plain-text `RuleViolation` with actionable hints (`expected` / `actual` / `fix`), including the corrected right-hand term.

Current EvalML1 checker/prover validates or generates derivation trees for CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`).
- `parser.rs` also parses judgment-only prover input (`... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`).
- `syntax.rs` models integers, booleans, expressions (`if`, `+`, `-`, `*`, `<`), and judgments (`evalto`, `plus is`, `minus is`, `times is`, `less than is`).
- `syntax.rs` includes `EvalML1Derivation` rendering in checker-compatible textual form.
- `checker.rs` validates EvalML1 and builtin arithmetic/boolean rules (`E-Int`, `E-Bool`, `E-IfT`, `E-IfF`, `E-Plus`, `E-Minus`, `E-Times`, `E-Lt`, `B-Plus`, `B-Minus`, `B-Times`, `B-Lt`).
- `prover.rs` deterministically constructs EvalML1 derivations by recursively evaluating expressions and emitting matching `E-*`/`B-*` rule nodes.
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`).
- Non-derivable prover judgments are reported as plain-text `RuleViolation` with actionable hints (`expected` / `actual` / `fix`) where derivation expectations are computable.

Current EvalML1Err checker/prover validates or generates derivation trees for CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`).
- `parser.rs` also parses judgment-only prover input (`... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`; values include `error`).
- `syntax.rs` extends EvalML1 value forms with `error`, keeps expression/judgment modeling aligned with EvalML1, and includes `EvalML1ErrDerivation` rendering in checker-compatible textual form.
- `checker.rs` validates EvalML1Err and builtin arithmetic/boolean rules (`E-*`, `B-*`), including error-propagation and type-error rules.
- `prover.rs` deterministically constructs EvalML1Err derivations by recursively evaluating expressions and emitting matching `E-*`/`B-*` rule nodes, including type-error and error-propagation branches.
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`).
- Non-derivable prover judgments are reported as plain-text `RuleViolation` with actionable hints (`expected` / `actual` / `fix`) where derivation expectations are computable.

Current EvalML2 checker/prover validates or generates derivation trees for CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`) and parses environments (`x = v, y = v' |- ...`).
- `parser.rs` also parses judgment-only prover input (`Gamma |- ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`).
- `syntax.rs` models integers, booleans, variables, `let`, arithmetic/conditional expressions, environments, and judgments.
- `syntax.rs` includes `EvalML2Derivation` rendering in checker-compatible textual form.
- `checker.rs` validates EvalML2 and builtin arithmetic/boolean rules (`E-Int`, `E-Bool`, `E-Var1`, `E-Var2`, `E-IfT`, `E-IfF`, `E-Let`, `E-Plus`, `E-Minus`, `E-Times`, `E-Lt`, `B-Plus`, `B-Minus`, `B-Times`, `B-Lt`).
- `prover.rs` deterministically constructs EvalML2 derivations by recursively evaluating expressions/environments and emitting matching `E-*`/`B-*` rule nodes.
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`Gamma |- ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`).
- Non-derivable prover judgments are reported as plain-text `RuleViolation` with actionable hints (`expected` / `actual` / `fix`) where derivation expectations are computable.

Current EvalML3 checker/prover validates or generates derivation trees for CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`) and parses environments (`x = v, y = v' |- ...`), closures (`(env)[fun x -> e]`), and recursive closures (`(env)[rec f = fun x -> e]`).
- `parser.rs` also parses judgment-only prover input (`Gamma |- ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`).
- `syntax.rs` models EvalML3 expressions and values, including `let rec`, `fun`, and application.
- `syntax.rs` includes `EvalML3Derivation` rendering in checker-compatible textual form.
- `checker.rs` validates EvalML3 and builtin arithmetic/boolean rules (`E-Int`, `E-Bool`, `E-Var1`, `E-Var2`, `E-IfT`, `E-IfF`, `E-Let`, `E-LetRec`, `E-Fun`, `E-App`, `E-AppRec`, `E-Plus`, `E-Minus`, `E-Times`, `E-Lt`, `B-Plus`, `B-Minus`, `B-Times`, `B-Lt`).
- `prover.rs` deterministically constructs EvalML3 derivations by recursively evaluating expressions/environments and emitting matching `E-*`/`B-*` rule nodes.
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`Gamma |- ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`).
- Non-derivable prover judgments are reported as plain-text `RuleViolation` with actionable hints (`expected` / `actual` / `fix`) where derivation expectations are computable.

Current EvalML4 checker/prover validates or generates derivation trees for CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`) and parses list/match syntax (`[]`, `::`, `match ... with [] -> ... | x :: y -> ...`).
- `parser.rs` also parses judgment-only prover input (`Gamma |- ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`).
- `syntax.rs` models EvalML4 expressions/values, extending EvalML3 with list values and match expressions.
- `syntax.rs` includes `EvalML4Derivation` rendering in checker-compatible textual form.
- `checker.rs` validates EvalML4 and builtin arithmetic/boolean rules (`E-Int`, `E-Bool`, `E-Var`, `E-IfT`, `E-IfF`, `E-Let`, `E-LetRec`, `E-Fun`, `E-App`, `E-AppRec`, `E-Nil`, `E-Cons`, `E-MatchNil`, `E-MatchCons`, `E-Plus`, `E-Minus`, `E-Times`, `E-Lt`, `B-Plus`, `B-Minus`, `B-Times`, `B-Lt`).
- `prover.rs` deterministically constructs EvalML4 derivations by recursively evaluating expressions/environments and emitting matching `E-*`/`B-*` rule nodes, including list and match branches.
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`Gamma |- ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`).
- Non-derivable prover judgments are reported as plain-text `RuleViolation` with actionable hints (`expected` / `actual` / `fix`) where derivation expectations are computable.

Current EvalML5 checker/prover validates or generates derivation trees for CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`) and parses multi-clause pattern matching (`match e with p1 -> e1 | p2 -> e2 | ...`), including wildcard/cons patterns and pattern-matching judgments.
- `parser.rs` also parses judgment-only prover input (`Gamma |- ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`, `p matches v when (...)`, `p doesn't match v`).
- `syntax.rs` models EvalML5 expressions/values, list patterns, match clauses, and pattern judgments (`matches`, `doesn't match`), and includes `EvalML5Derivation` rendering in checker-compatible textual form.
- `checker.rs` validates EvalML5, pattern matching rules, and builtin arithmetic/boolean rules (`E-*`, `M-*`, `NM-*`, `B-*`), including `E-MatchM1`, `E-MatchM2`, and `E-MatchN`.
- `prover.rs` deterministically constructs EvalML5 derivations by recursively evaluating expressions/patterns and emitting matching `E-*` / `M-*` / `NM-*` / `B-*` rule nodes.
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`Gamma |- ... evalto ...`, `p matches v when (...)`, `p doesn't match v`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`).
- Non-derivable prover judgments are reported as plain-text `RuleViolation` with actionable hints (`expected` / `actual` / `fix`) where derivation expectations are computable.

Current EvalContML1 checker validates derivation trees parsed from CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`) and parses continuation judgments (`>>`, `=>`, `_`) and continuation frames (`{_ op e}`, `{i op _}`, `{if _ then e2 else e3}`).
- `syntax.rs` models EvalML1-style expressions/values extended with continuation structures and continuation-application judgments.
- `checker.rs` validates continuation evaluation and builtin arithmetic/boolean rules (`E-Int`, `E-Bool`, `E-BinOp`, `E-If`, `C-Ret`, `C-EvalR`, `C-Plus`, `C-Minus`, `C-Times`, `C-Lt`, `C-IfT`, `C-IfF`, `B-Plus`, `B-Minus`, `B-Times`, `B-Lt`).
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`... evalto ...`, `... >> ... evalto ...`, `... => ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`).

Current EvalContML4 checker validates derivation trees parsed from CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`) and parses continuation judgments (`>>`, `=>`, `_`), continuation frames with optional environment prefixes (`{Gamma |- ...}`), `letcc`, closures/recursive closures, lists, and match expressions.
- `syntax.rs` models EvalML4-style expressions/values extended with continuation values (`[k]`) and continuation-application judgments (`v => k evalto v'`).
- `checker.rs` validates continuation evaluation, environment/list/match/function rules, and builtin arithmetic/boolean rules (`E-*`, `C-*`, `B-*`) including `E-LetCc`, `C-EvalFun`, `C-EvalFunR`, `C-EvalFunC`, `C-EvalConsR`, `C-Cons`, and `C-MatchCons`.
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`Gamma |- ... evalto ...`, `... >> ... evalto ...`, `... => ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`).

Current TypingML4 checker validates derivation trees parsed from CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`) and parses type environments (`x : t`), function/list types (`t1 -> t2`, `t list`), and list-match expressions.
- `syntax.rs` models TypingML4 expressions, types, type environments, and typing judgments (`Gamma |- e : t`).
- `checker.rs` validates TypingML4 typing rules (`T-Int`, `T-Bool`, `T-Var`, `T-If`, `T-Plus`, `T-Minus`, `T-Times`, `T-Lt`, `T-Let`, `T-Fun`, `T-App`, `T-LetRec`, `T-Nil`, `T-Cons`, `T-Match`).
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`Gamma |- e : t`).

Current PolyTypingML4 checker validates derivation trees parsed from CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`) and parses polymorphic type schemes in environments (`'a ... . t`) as well as type variables (`'a`).
- `syntax.rs` models PolyTypingML4 expressions, polymorphic type syntax, type schemes, type environments, and typing judgments (`Gamma |- e : t`).
- `checker.rs` validates PolyTypingML4 typing rules (`T-Int`, `T-Bool`, `T-Var`, `T-If`, `T-Plus`, `T-Minus`, `T-Mult`, `T-Lt`, `T-Let`, `T-Abs`, `T-App`, `T-LetRec`, `T-Nil`, `T-Cons`, `T-Match`) including:
  - type-scheme instantiation checks for `T-Var`
  - let-generalization checks for `T-Let` and `T-LetRec`
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`Gamma |- e : t`).

Current NamelessML3 checker validates derivation trees parsed from CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`) and parses translation judgments (`Gamma |- e ==> e'`) for named and nameless ML3 expressions.
- `syntax.rs` models named expressions (`if`, `let`, `let rec`, `fun`, application, arithmetic) and nameless expressions (`#n`, `let . =`, `fun . ->`, `let rec . = fun . ->`) separately.
- `checker.rs` validates translation rules (`Tr-Int`, `Tr-Bool`, `Tr-Var1`, `Tr-Var2`, `Tr-If`, `Tr-Plus`, `Tr-Minus`, `Tr-Times`, `Tr-Lt`, `Tr-Let`, `Tr-Fun`, `Tr-App`, `Tr-LetRec`).
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`Gamma |- e ==> e'`).

Current EvalNamelessML3 checker validates derivation trees parsed from CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`) and parses nameless-evaluation judgments (`Gamma |- e evalto v`) with de Bruijn indices (`#n`).
- `syntax.rs` models nameless expressions (`let . =`, `fun . ->`, `let rec . = fun . ->`, application) and closure values (`(Gamma)[fun . -> e]`, `(Gamma)[rec . = fun . -> e]`).
- `checker.rs` validates EvalNamelessML3 and builtin arithmetic/boolean rules (`E-Int`, `E-Bool`, `E-Var`, `E-IfT`, `E-IfF`, `E-Let`, `E-LetRec`, `E-Fun`, `E-App`, `E-AppRec`, `E-Plus`, `E-Minus`, `E-Times`, `E-Lt`, `B-Plus`, `B-Minus`, `B-Times`, `B-Lt`).
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`Gamma |- e evalto v`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`).

Current EvalNatExp checker validates derivation trees parsed from CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`).
- `syntax.rs` models expressions (`n`, `e + e`, `e * e`), Nat terms, and judgments (`evalto`, `plus is`, `times is`).
- `checker.rs` validates EvalNatExp and embedded Nat rules (`E-Const`, `E-Plus`, `E-Times`, `P-Zero`, `P-Succ`, `T-Zero`, `T-Succ`).
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`... evalto ...`, `... plus ... is ...`, `... times ... is ...`).

Current ReduceNatExp checker validates derivation trees parsed from CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`).
- `syntax.rs` models expressions (`n`, `e + e`, `e * e`), Nat terms, and judgments (`--->`, `-d->`, `-*->`, `plus is`, `times is`).
- `checker.rs` validates ReduceNatExp and embedded Nat rules (`R-*`, `DR-*`, `MR-*`, `P-Zero`, `P-Succ`, `T-Zero`, `T-Succ`).
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`), failing premise path (`root`, `1`, `1.2`, ...), and actionable hints where available.
- Successful check result text is the inferred root judgment (`... ---> ...`, `... -d-> ...`, `... -*-> ...`, `... plus ... is ...`, `... times ... is ...`).

## 8. Extension Strategy

### 8.1 Adding new games

Current extension path is:

1. Add a new variant to `GameKind`.
2. Update `TryFrom<&str>` for CLI parsing.
3. Add game module under `src/games/<new_game>/`.
4. Extend `games::run_checker` match dispatch.

This keeps game addition mostly localized to registry points and one module tree.
Detailed step-by-step instructions are documented in `docs/how-to-add-a-game.md`.

### 8.2 Extending `prover`

Current status:

- `prover` is wired in CLI parsing and `lib::execute`.
- `prover --game Nat` parses judgment-only input (`plus/times ... is ...`), constructs derivation ASTs, and prints checker-compatible derivations.
- `prover --game CompareNat1` parses judgment-only input (`... is less than ...`), constructs derivation ASTs, and prints checker-compatible derivations.
- `prover --game CompareNat2` parses judgment-only input (`... is less than ...`), constructs derivation ASTs, and prints checker-compatible derivations.
- `prover --game CompareNat3` parses judgment-only input (`... is less than ...`), constructs derivation ASTs, and prints checker-compatible derivations.
- `prover --game EvalML1` parses judgment-only input (`evalto` / `plus` / `minus` / `times` / `less than`), constructs derivation ASTs, and prints checker-compatible derivations.
- `prover --game EvalML1Err` parses judgment-only input (`evalto` / `plus` / `minus` / `times` / `less than`, with `error` value), constructs derivation ASTs, and prints checker-compatible derivations.
- `prover --game EvalML2` parses judgment-only input (`Gamma |- ... evalto ...` / `plus` / `minus` / `times` / `less than`), constructs derivation ASTs, and prints checker-compatible derivations.
- `prover --game EvalML3` parses judgment-only input (`Gamma |- ... evalto ...` / `plus` / `minus` / `times` / `less than`), constructs derivation ASTs, and prints checker-compatible derivations.
- `prover --game EvalML4` parses judgment-only input (`Gamma |- ... evalto ...` / `plus` / `minus` / `times` / `less than`), constructs derivation ASTs, and prints checker-compatible derivations.
- `prover --game EvalML5` parses judgment-only input (`Gamma |- ... evalto ...` / `plus` / `minus` / `times` / `less than` / pattern `matches` / pattern `doesn't match`), constructs derivation ASTs, and prints checker-compatible derivations.
- `prover` for unsupported games returns `RunError::ProverNotImplemented`.

Re-evaluation result (2026-02-19):

- Do not introduce a shared proof-search core at this point.
- Keep game-specific prover implementations (`Nat`, `CompareNat1`, `CompareNat2`, `CompareNat3`, `EvalML1`, `EvalML1Err`, `EvalML2`, `EvalML3`, `EvalML4`, `EvalML5`) as the default strategy.
- Rationale:
  - The ten provers are deterministic, rule-driven evaluators, but their core domains differ (`Nat` arithmetic recursion, `CompareNat1` transitive less-than chaining, `CompareNat2` structural less-than recursion, `CompareNat3` right-side successor recursion, `EvalML1` expression evaluation, `EvalML1Err` error-aware evaluation, `EvalML2` environment-sensitive evaluation, `EvalML3` environment/closure-sensitive evaluation, `EvalML4` list/match-aware evaluation, and `EvalML5` multi-clause/pattern-relation-aware evaluation).
  - Current overlap is limited to small helper patterns and does not justify a new cross-game proof-search abstraction.
  - Introducing a generic core now would increase coupling across game modules without a proportional reduction in implementation complexity.
- Revisit this decision when one of the following becomes true:
  - A new prover requires non-deterministic search/backtracking beyond current deterministic evaluators.
  - Shared prover logic grows enough that local helper extraction is no longer sufficient.
  - Adding a new prover repeatedly requires touching multiple existing prover modules for cross-cutting behavior.

## 9. Quality Gates

Standard pre-commit checks:

- `cargo fmt`
- `cargo test`
- `cargo clippy --all-targets --all-features -- -D warnings`
- Backlog task completion (`[ ] -> [x]` in `docs/PLAN.md`) is finalized in one flow via the `task-commit` skill after validation.

Development style follows TDD (`Red -> Green -> Refactor`), then a 5-iteration `review -> improve` loop after implementation.
Review criteria are aligned with `AGENTS.md` Design Principles (`high cohesion/low coupling`, `YAGNI`, `KISS`).
Project planning is managed in a single prioritized backlog in `docs/PLAN.md`, where implementation and improvement tasks are ordered explicitly for execution.
Completed or frozen plans are archived under `docs/plans/`.

## 10. Open Items

- Prioritized implementation/improvement work is managed in `docs/PLAN.md` backlog.
- Decide output schema for machine-readable mode (if JSON is introduced later).

## 11. Current Direction Note

- Nat derivations are parsed into a generic tree (`judgment + raw rule name + subderivations`).
- Nat parser/checker boundary stays non-rule-indexed; premise shape checks are done in checker rule matching (ADR-0005).
- Nat judgments are represented by explicit enum forms (`PlusIs` / `TimesIs`) rather than a shared operator field (ADR-0007).
- Premise arity mismatches are treated as rule-validation failures in checker (`RuleViolation`), not parse errors (ADR-0005).
- Checker inconsistency diagnostics carry failing-node `SourceSpan` (`line:column`) (ADR-0006).
- Successful checker output is aligned with the reference implementation by printing the inferred root judgment text directly (ADR-0008).
- EvalML1 prover is implemented with game-specific recursive evaluation that deterministically emits `E-*` / `B-*` derivations and checker-compatible pretty-printed output.
- CompareNat1 prover is implemented with game-specific recursive proof construction that deterministically emits `L-Succ` / `L-Trans` derivations and checker-compatible pretty-printed output.
- CompareNat1 checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- CompareNat2 prover is implemented with game-specific recursive proof construction that deterministically emits `L-Zero` / `L-SuccSucc` derivations and checker-compatible pretty-printed output.
- CompareNat2 checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- CompareNat3 prover is implemented with game-specific recursive proof construction that deterministically emits `L-Succ` / `L-SuccR` derivations and checker-compatible pretty-printed output.
- CompareNat3 checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- EvalML1 checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- EvalML1Err checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- EvalML1Err prover is implemented with game-specific recursive evaluation that deterministically emits `E-*` / `B-*` derivations, including type-error and error-propagation branches, and checker-compatible pretty-printed output.
- EvalML2 checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- EvalML2 prover is implemented with game-specific recursive evaluation that deterministically emits `E-*` / `B-*` derivations and checker-compatible pretty-printed output.
- EvalML3 checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- EvalML3 prover is implemented with game-specific recursive evaluation that deterministically emits `E-*` / `B-*` derivations and checker-compatible pretty-printed output.
- EvalML4 checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- EvalML4 prover is implemented with game-specific recursive evaluation that deterministically emits `E-*` / `B-*` derivations, including list/match rules, and checker-compatible pretty-printed output.
- EvalML5 checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- EvalML5 prover is implemented with game-specific recursive evaluation/pattern resolution that deterministically emits `E-*` / `M-*` / `NM-*` / `B-*` derivations and checker-compatible pretty-printed output.
- EvalContML1 checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- EvalContML4 checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- TypingML4 checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- PolyTypingML4 checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- NamelessML3 checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- EvalNamelessML3 checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- EvalNatExp checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- ReduceNatExp checker is implemented with the same parser/checker boundary policy as Nat (raw rule names in parser, rule resolution in checker).
- ReduceNatExp checker centralizes shared validation paths for `R-*` and `DR-*` rules by switching relation kind (`--->` / `-d->`) and operator kind (`+` / `*`) in common helper functions.
- Nat arithmetic rule-shape checks (`P-Zero`, `P-Succ`, `T-Zero`, `T-Succ`) are centralized in `src/games/nat_arith.rs` and reused by `Nat`, `EvalNatExp`, and `ReduceNatExp`.
