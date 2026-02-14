# copl-rs Design (Snapshot)

Last updated: 2026-02-14

## 1. Purpose

This document captures the current architecture and design decisions for `copl-rs`.
It is a snapshot of the implementation state and the agreed extension direction.

## 2. Scope

### In scope now

- Single-crate Rust project.
- CLI entry point as `copl-rs`.
- `checker` subcommand with unified game selection:
  - `copl-rs checker --game <name> [file]`
- Current game target: `nat`.

### Out of scope now

- `resolver` command implementation.
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
  - CLI domain model and argument parsing.
- `src/core/mod.rs`:
  - Shared domain contracts (`GameKind`, `Game`, `CheckReport`, `CheckError`).
- `src/games/mod.rs`:
  - Game registry/dispatch via `enum GameKind + match`.
- `src/games/nat/`:
  - `syntax.rs`, `parser.rs`, `checker.rs`.

This structure is intended to localize game-specific logic under `games/<game>/`.

## 4. CLI Design

### 4.1 Command model

`Cli` uses a typed command tree:

- `Cli { command: Command }`
- `Command::Checker(CheckerCommand)`
- `CheckerCommand { game: GameKind, input: InputSource }`
- `InputSource::Stdin | InputSource::File(PathBuf)`

### 4.2 Parsing approach

Checker argument parsing is implemented as a typed state machine.

- Internal parser state uses typestate-like markers:
  - mode-like markers: `ModeOptions`, `ModePositional`
  - expectation markers: `ExpectAny`, `ExpectGameValue`
- Token classification in option mode is separated by `classify_options_event(...)`.
- Input processing uses `try_fold` over argument tokens.

This avoids index-mutation style loops and keeps transitions explicit.

### 4.3 Compatibility guarantees

Current compatibility rules:

- Keep `copl-rs checker --game <name> [file]` behavior stable.
- If `[file]` is omitted, read from `stdin`.
- Support `--` to treat following `-`-prefixed tokens as positional file names.

## 5. Runtime Flow

`main -> lib::run -> cli::parse -> execute -> games::run_checker`

Key runtime checks in `lib.rs`:

- Input source selection (`stdin` or file).
- Maximum input size:
  - test build: 1024 bytes
  - non-test build: 8 MiB
- UTF-8 validation.

## 6. Error Model

- CLI parse errors are represented by `CliError`.
- Checker-level errors use `core::CheckError` with:
  - `CheckErrorKind` (`Parse`, `RuleViolation`, `Internal`)
  - optional `SourceSpan`.
- Top-level runtime errors are wrapped in `RunError`.

Error output is currently plain text.

## 7. Nat Implementation Status

Current Nat checker validates derivation trees parsed from CoPL ASCII input.

- `parser.rs` builds a generic derivation tree (`judgment + raw rule name + subderivations`).
- `syntax.rs` models Nat judgments as an enum with explicit forms (`PlusIs`, `TimesIs`) (ADR-0007).
- `checker.rs` validates Nat rule constraints (`P-Zero`, `P-Succ`, `T-Zero`, `T-Succ`).
- Rule definitions are fixed and referenced statically from checker-local rule IDs.
- Rule application checks are written as per-rule pattern matching over `subderivations`, aligned with the upstream checker style (ADR-0007).
- Rule names are stored as raw text in the parsed tree and matched to static rule definitions in checker.
- Unknown rule names and premise arity mismatches are reported as `RuleViolation`.
- `RuleViolation` diagnostics carry the derivation node source location (`SourceSpan`).

## 8. Extension Strategy

### 8.1 Adding new games

Current extension path is:

1. Add a new variant to `GameKind`.
2. Update `TryFrom<&str>` for CLI parsing.
3. Add game module under `src/games/<new_game>/`.
4. Extend `games::run_checker` match dispatch.

This keeps game addition mostly localized to registry points and one module tree.
Detailed step-by-step instructions are documented in `docs/how-to-add-a-game.md`.

### 8.2 Adding `resolver`

Planned direction:

- Add a new subcommand variant under `Command`.
- Add resolver route in `Cli::parse`.
- Implement execution path in `lib::execute`.
- Reuse `--game <name>` convention for consistency.

## 9. Quality Gates

Standard pre-commit checks:

- `cargo fmt`
- `cargo test`
- `cargo clippy --all-targets --all-features -- -D warnings`

Development style follows TDD (`Red -> Green -> Refactor`).

## 10. Open Items

- Improve diagnostics to pinpoint failing premise paths more explicitly.
- Expand invalid-case fixtures for broader rule-violation coverage.
- Decide if CLI parsing should be split per-subcommand module before adding `resolver`.
- Decide output schema for machine-readable mode (if JSON is introduced later).

## 11. Current Direction Note

- Nat derivations are parsed into a generic tree (`judgment + raw rule name + subderivations`).
- Nat parser/checker boundary stays non-rule-indexed; premise shape checks are done in checker rule matching (ADR-0005).
- Nat judgments are represented by explicit enum forms (`PlusIs` / `TimesIs`) rather than a shared operator field (ADR-0007).
- Premise arity mismatches are treated as rule-validation failures in checker (`RuleViolation`), not parse errors (ADR-0005).
- Checker inconsistency diagnostics carry failing-node `SourceSpan` (`line:column`) (ADR-0006).
