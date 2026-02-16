# ADR-0001: Use a Single-Crate Architecture with Explicit Module Boundaries

- Status: Accepted
- Date: 2026-02-07

## Context

The project is in an early stage and currently targets a Nat checker.
We need fast iteration while keeping boundaries clear enough for future additions (new games, prover command).

## Decision

Use one Rust crate for now.

- Keep `src/main.rs` as a thin CLI entry point only.
- Place application logic in `src/lib.rs`.
- Use explicit module boundaries:
  - `cli`
  - `core`
  - `games`
  - `games/nat` (`syntax`, `parser`, `checker`)

## Consequences

### Positive

- Simple build/test workflow.
- Easier refactoring in early development.
- Clear separation between entrypoint, orchestration, shared contracts, and game-specific logic.

### Negative

- If components grow independently, crate-level isolation may eventually be needed.
- We may revisit this decision when `prover` and additional games are implemented.

