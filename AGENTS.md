# AGENTS.md

This file records the agreed development policies for this repository.

Last updated: 2026-02-19

## Scope Ownership

- Active implementation scope (target games, order, pending decisions) is managed only in `docs/PLAN.md`.
- `AGENTS.md` defines repository-wide policies and must not duplicate active task scope lists.
- `prover` remains out of scope until it is added to `docs/PLAN.md`.

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
- `checker` is implemented.
- `prover` is implemented for `Nat` (`copl-rs prover --game Nat [file]`).
- `prover` is implemented for `CompareNat1` (`copl-rs prover --game CompareNat1 [file]`).
- `prover` is implemented for `CompareNat2` (`copl-rs prover --game CompareNat2 [file]`).
- `prover` is implemented for `CompareNat3` (`copl-rs prover --game CompareNat3 [file]`).
- `prover` is implemented for `EvalML1` (`copl-rs prover --game EvalML1 [file]`).
- `prover` is implemented for `EvalML1Err` (`copl-rs prover --game EvalML1Err [file]`).
- `prover` is implemented for `EvalML2` (`copl-rs prover --game EvalML2 [file]`).
- `prover` is implemented for `EvalML3` (`copl-rs prover --game EvalML3 [file]`).
- For games other than `Nat` / `CompareNat1` / `CompareNat2` / `CompareNat3` / `EvalML1` / `EvalML1Err` / `EvalML2` / `EvalML3`, `prover` currently returns a not-implemented error.
- Game selection is unified as `--game <name>`.
- Expected command shape:
  - `copl-rs checker --game Nat <file>`
  - `copl-rs checker --game CompareNat1 <file>`
  - `copl-rs checker --game CompareNat2 <file>`
  - `copl-rs checker --game CompareNat3 <file>`
  - `copl-rs checker --game EvalML1 <file>`
  - `copl-rs checker --game EvalNatExp <file>`
  - `copl-rs checker --game ReduceNatExp <file>`
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
- `games/nat` is split into `syntax` / `parser` / `checker` / `prover`.
- `games/compare_nat1` is split into `syntax` / `parser` / `checker` / `prover`.
- `games/compare_nat2` is split into `syntax` / `parser` / `checker` / `prover`.
- `games/compare_nat3` is split into `syntax` / `parser` / `checker` / `prover`.
- `games/eval_ml1` is split into `syntax` / `parser` / `checker` / `prover`.
- `games/eval_ml1_err` is split into `syntax` / `parser` / `checker` / `prover`.
- `games/eval_ml2` is split into `syntax` / `parser` / `checker` / `prover`.
- `games/eval_ml3` is split into `syntax` / `parser` / `checker` / `prover`.
- `games/eval_nat_exp` is split into `syntax` / `parser` / `checker`.
- `games/reduce_nat_exp` is split into `syntax` / `parser` / `checker`.
- Use `enum GameKind + match` for game registry/dispatch.

## Design Principles

- Prioritize high cohesion and low coupling:
  - Keep responsibilities focused within modules and minimize unnecessary cross-module dependencies.
- Prioritize `YAGNI`:
  - Do not introduce features, extension points, or abstractions before a current task requires them.
- Prioritize `KISS`:
  - Prefer the simplest design that satisfies current requirements and keeps behavior easy to reason about.
- When principles conflict, resolve in this order:
  - High cohesion/low coupling -> `YAGNI` -> `KISS` (incremental application).
- Use the following design/review questions:
  - High cohesion / low coupling:
    - Does this module handle only one concept?
    - How far does the impact of change propagate?
  - `YAGNI`:
    - Is this extension used by current requirements?
    - Is this abstraction for uncertain future requirements?
    - Can we explain what would break if we remove this now?
  - `KISS`:
    - Is there a simpler implementation?
    - Can we remove one layer or abstraction?
    - Does reading this require avoidable prerequisite knowledge?

## Development Process

- Follow t-wada style TDD.
- Base cycle: `Red -> Green -> Refactor`.
- Start from the smallest failing test, implement the minimum to pass, then refactor.
- After implementation is complete, repeat a `review -> improve` loop 5 times.
- In each review loop, review code using the design/review questions in the Design Principles section.
- Findings raised in review loops must be handled as follows:
  - If a finding is within the current task scope, fix it before completing the task.
  - If a finding is outside the current task scope, add it to the backlog in `docs/PLAN.md` with an appropriate priority/order.

## Task Completion

- After meeting the Definition of Done, run the `retrospect` skill for documentation sync and retrospective review.
- When marking a backlog task done (`[ ] -> [x]` in `docs/PLAN.md`), run the `task-commit` skill so task checks and commit creation are handled in one flow.
- Before committing, follow the checks in the `Pre-Commit Validation` section. If any check is intentionally skipped, document the reason in the commit message.

## ADR Policy

- Create or update an ADR under `docs/adr/` when any of the following changes:
  - Public CLI contract (command shape, options, compatibility rules)
  - Module boundaries or ownership of major components
  - Application development process and quality gates (build/test/lint) that affect project delivery
- Include `Context`, `Decision`, and `Consequences` in each ADR.
- Do not record Codex/agent operation behavior in `docs/adr/`.

## Documentation Sync Policy

- When implementation changes architecture, command behavior, or operational policy:
  - Update `docs/design.md` in the same PR/commit set.
  - Update `README.md` if user-facing behavior or setup changes.
  - Update `docs/PLAN.md` when milestone status changes.
  - Move completed or frozen plans to `docs/plans/`.

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
- Rule definition: https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/CompareNat3.html
- Rule definition: https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/EvalML1.html
- Rule definition: https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/EvalML1Err.html
- Rule definition: https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/EvalML2.html
- Rule definition: https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/EvalML3.html
- Rule definition: https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/EvalNatExp.html
- Rule definition: https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/ReduceNatExp.html
- ASCII examples:
  - Nat: `copl/001.copl` to `copl/008.copl`
  - CompareNat1: `copl/009.copl`, `copl/012.copl`
  - CompareNat2: `copl/010.copl`, `copl/013.copl`
  - CompareNat3: `copl/011.copl`, `copl/014.copl`
  - EvalML1: `copl/025.copl` to `copl/030.copl`
  - EvalML1Err: `copl/031.copl` to `copl/033.copl`
  - EvalML2: `copl/034.copl` to `copl/039.copl`
  - EvalML3: `copl/040.copl` to `copl/053.copl`
  - EvalNatExp: `copl/015.copl` to `copl/020.copl`
  - ReduceNatExp: `copl/021.copl` to `copl/024.copl`

## Operations

- Keep progress details updated in `docs/PLAN.md`.
- Manage implementation tasks and improvement tasks in one prioritized backlog in `docs/PLAN.md`, with explicit top-to-bottom start order.
- When improvements outside the current task scope are discovered during task execution, add them to the backlog in `docs/PLAN.md` and insert them at an appropriate position after considering priority and dependencies.
- Keep historical plans under `docs/plans/` and avoid mixing archived details into the current-plan section of `docs/PLAN.md`.
- Keep architecture and design decisions updated in `docs/design.md`.
- Keep architecture decision records in `docs/adr/`.
- Do not duplicate procedure-level workflow instructions in repository documents.
- If policies change, update this file and `docs/PLAN.md` when relevant.

## Definition of Done

- A task is considered done when all applicable items are completed:
  - Code changes are implemented.
  - Tests for the changed behavior are added/updated.
  - Related docs are updated (`README.md`, `docs/design.md`, `docs/PLAN.md`, ADRs as needed).
  - Review log is completed for `R1` to `R5` (`review -> improve`), including explicit `no finding` entries when applicable.
  - Review findings are fully handled: in-scope findings are fixed, and out-of-scope findings are added to the backlog.
  - Validation gates pass (`cargo fmt`, `cargo test`, `cargo clippy --all-targets --all-features -- -D warnings`).
