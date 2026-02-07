# ADR-0004: Adopt t-wada Style TDD and Pre-Commit Validation Gates

- Status: Accepted
- Date: 2026-02-07

## Context

The project is expected to grow incrementally while preserving correctness.
To keep iteration safe, we need a repeatable development loop and objective quality gates.

## Decision

Use t-wada style TDD as the default development process.

- Base cycle: `Red -> Green -> Refactor`.
- Start from the smallest failing test.
- Implement the minimum change to pass.
- Refactor only while tests remain green.

Run the following validation commands before commit by default:

- `cargo fmt`
- `cargo test`
- `cargo clippy --all-targets --all-features -- -D warnings`

If any gate is intentionally skipped, record the reason in the commit message.

## Consequences

### Positive

- Reduces regression risk during incremental changes.
- Improves confidence in refactoring-heavy work.
- Keeps code quality decisions explicit and auditable.

### Negative

- Adds process overhead for very small changes.
- Requires discipline to keep cycles small and focused.

