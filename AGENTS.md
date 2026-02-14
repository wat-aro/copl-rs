# AGENTS.md

This file records the agreed development policies for this repository.

Last updated: 2026-02-14

## Scope

- The current implementation targets are CoPL `Nat`, `CompareNat1`, and `CompareNat2` checkers.
- `resolver` will be added later (out of scope for now).

## Language Policy

- Repository documents must be written in English.
- This includes at least:
  - `README.md`
  - `AGENTS.md`
- Exception:
  - `docs/PLAN.md` and files under `docs/plans/` are maintained in Japanese.

## CLI Policy

- Binary name: `copl-rs`
- Use subcommand style.
- For now, only `checker` is implemented.
- Game selection is unified as `--game <name>`.
- Expected command shape:
  - `copl-rs checker --game Nat <file>`
  - `copl-rs checker --game CompareNat1 <file>`
  - `copl-rs checker --game CompareNat2 <file>`
  - If `<file>` is omitted, read from `stdin`.
- Keep game-name input case-insensitive for backward compatibility.
- On success, checker output should be the inferred root judgment text in plain text.

## CLI Compatibility

- Keep `copl-rs checker --game <name> [file]` backward-compatible.
- Preserve `stdin` behavior when `[file]` is omitted.
- Keep `--` delimiter support for file names that start with `-`.

## Implementation Policy

- Start with a single crate.
- Keep `src/main.rs` focused on CLI entry; place logic in `src/lib.rs`.
- Module boundaries: `cli` / `core` / `games`.
- `games/nat` is split into `syntax` / `parser` / `checker`.
- `games/compare_nat1` is split into `syntax` / `parser` / `checker`.
- `games/compare_nat2` is split into `syntax` / `parser` / `checker`.
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
  - Update `docs/PLAN.md` (index) and the active plan under `docs/plans/` when milestone status changes.

## Pre-Commit Validation

- Run these checks by default before committing:
  - `cargo fmt`
  - `cargo test`
  - `cargo clippy --all-targets --all-features -- -D warnings`
- If any check is intentionally skipped, document the reason in the commit message.

## Validation and Error Policy

- Use a hand-written recursive-descent parser.
- Unknown rule names should be reported as `RuleViolation` errors during checker rule resolution.
- Checker inconsistency errors should include source location (`line:column`) of the failing derivation node.
- `RuleViolation` diagnostics should include actionable hints (`expected` / `actual` / `fix`) where available.
- Error output is plain text first (JSON mode can be considered later).

## Input Specification References

- Rule definition: https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/Nat.html
- Rule definition: https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/CompareNat1.html
- Rule definition: https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/CompareNat2.html
- ASCII examples:
  - Nat: `copl/001.copl` to `copl/008.copl`
  - CompareNat1: `copl/009.copl`, `copl/012.copl`
  - CompareNat2: `copl/010.copl`, `copl/013.copl`

## Operations

- Keep progress details updated in `docs/PLAN.md` and the active plan under `docs/plans/`.
- Keep architecture and design decisions updated in `docs/design.md`.
- Keep architecture decision records in `docs/adr/`.
- Codex should refer to `docs/PLAN.md` and follow the linked active plan under `docs/plans/` when checking implementation plans.
- Codex should refer to `docs/design.md` when discussing design and refactoring decisions.
- Codex should refer to `docs/adr/` when discussing historical design tradeoffs.
- When a session reveals documentation or skill updates, Codex should propose candidate updates and ask for user confirmation before applying any changes.
- Use the `retrospect` skill for this workflow.
- The user can trigger it explicitly with `retrospect` or `$retrospect`.
- If policies change, update this file, `docs/PLAN.md`, and the active plan under `docs/plans/` when relevant.

## Definition of Done

- A task is considered done when all applicable items are completed:
  - Code changes are implemented.
  - Tests for the changed behavior are added/updated.
  - Related docs are updated (`README.md`, `docs/design.md`, `docs/PLAN.md`, active file in `docs/plans/`, ADRs as needed).
  - Validation gates pass (`cargo fmt`, `cargo test`, `cargo clippy --all-targets --all-features -- -D warnings`).
