# Contributing to copl-rs

This document describes how to develop and maintain `copl-rs`.

## Prerequisites

- Rust toolchain (`cargo`, `rustc`)
- Git (with submodule support)

If this is your first checkout:

```sh
git clone <this-repo>
cd copl-rs
git submodule update --init --recursive
```

## Development Workflow

- Follow t-wada style TDD: `Red -> Green -> Refactor`.
- Start with the smallest failing test, implement the minimum to pass, then refactor.
- Keep active scope and progress in `docs/PLAN.md`.
- Archive completed or frozen plans under `docs/plans/`.

## Validation Commands

Run these checks before committing:

```sh
cargo fmt
cargo test
cargo clippy --all-targets --all-features -- -D warnings
```

## Repository Structure

Core structure:

```text
src/main.rs          # CLI entry point
src/lib.rs           # Application flow
src/cli/             # CLI parsing and command dispatch
src/core/            # Shared types and error/report model
src/games/           # Per-game syntax/parser/checker implementations
docs/design.md       # Design snapshot
docs/PLAN.md         # Active prioritized plan
docs/plans/          # Archived plans
docs/adr/            # Architecture decision records
AGENTS.md            # Repository-wide development policies
```

## Documentation Responsibilities

When behavior or policy changes, update related docs in the same change set:

- `README.md`: user-facing usage and setup
- `docs/design.md`: architecture/module responsibilities
- `docs/PLAN.md` and `docs/plans/`: plan status/history
- `docs/adr/`: decisions affecting CLI contract, architecture boundaries, or process gates
- `AGENTS.md`: repository-wide policy updates
