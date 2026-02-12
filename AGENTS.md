# AGENTS.md

This file records the agreed development policies for this repository.

Last updated: 2026-02-12

## Scope

- The current implementation target is a CoPL `Nat` checker.
- `resolver` will be added later (out of scope for now).

## Language Policy

- Repository documents must be written in English.
- This includes at least:
  - `README.md`
  - `AGENTS.md`
- Exception:
  - `docs/PLAN.md` is maintained in Japanese.

## CLI Policy

- Binary name: `copl-rs`
- Use subcommand style.
- For now, only `checker` is implemented.
- Game selection is unified as `--game <name>`.
- Expected command shape:
  - `copl-rs checker --game nat <file>`
  - If `<file>` is omitted, read from `stdin`.

## CLI Compatibility

- Keep `copl-rs checker --game <name> [file]` backward-compatible.
- Preserve `stdin` behavior when `[file]` is omitted.
- Keep `--` delimiter support for file names that start with `-`.

## Implementation Policy

- Start with a single crate.
- Keep `src/main.rs` focused on CLI entry; place logic in `src/lib.rs`.
- Module boundaries: `cli` / `core` / `games`.
- `games/nat` is split into `syntax` / `parser` / `checker`.
- Use `enum GameKind + match` for game registry/dispatch.

## Development Process

- Follow t-wada style TDD.
- Base cycle: `Red -> Green -> Refactor`.
- Start from the smallest failing test, implement the minimum to pass, then refactor.

## ADR Policy

- Create or update an ADR under `docs/adr/` when any of the following changes:
  - Public CLI contract (command shape, options, compatibility rules)
  - Module boundaries or ownership of major components
  - Development process and quality gates that affect team workflow
- Include `Context`, `Decision`, and `Consequences` in each ADR.

## Documentation Sync Policy

- When implementation changes architecture, command behavior, or operational policy:
  - Update `docs/design.md` in the same PR/commit set.
  - Update `README.md` if user-facing behavior or setup changes.
  - Update `docs/PLAN.md` progress when milestone status changes.

## Pre-Commit Validation

- Run these checks by default before committing:
  - `cargo fmt`
  - `cargo test`
  - `cargo clippy --all-targets --all-features -- -D warnings`
- If any check is intentionally skipped, document the reason in the commit message.

## Validation and Error Policy

- Use a hand-written recursive-descent parser.
- Unknown rule names should be reported as `RuleViolation` errors during checker rule resolution.
- Error output is plain text first (JSON mode can be considered later).

## Input Specification References

- Rule definition: https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/Nat.html
- ASCII examples: `copl/001.copl` to `copl/008.copl`

## Operations

- Keep progress details updated in `docs/PLAN.md`.
- Keep architecture and design decisions updated in `docs/design.md`.
- Keep architecture decision records in `docs/adr/`.
- Codex should refer to `docs/PLAN.md` when checking implementation plans.
- Codex should refer to `docs/design.md` when discussing design and refactoring decisions.
- Codex should refer to `docs/adr/` when discussing historical design tradeoffs.
- If policies change, update both this file and `docs/PLAN.md`.

## Definition of Done

- A task is considered done when all applicable items are completed:
  - Code changes are implemented.
  - Tests for the changed behavior are added/updated.
  - Related docs are updated (`README.md`, `docs/design.md`, `docs/PLAN.md`, ADRs as needed).
  - Validation gates pass (`cargo fmt`, `cargo test`, `cargo clippy --all-targets --all-features -- -D warnings`).
