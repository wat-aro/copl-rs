# How to Add a New Game

This guide describes the minimum steps to add a new CoPL game checker to `copl-rs`.

## 1. Add a new game kind

Edit `src/core/mod.rs`:

- Add a new variant to `GameKind`.
- Add its lowercase name to `GameKind::as_str`.
- Accept that name in `TryFrom<&str> for GameKind`.

## 2. Create a game module

Create `src/games/<new_game>/` with:

- `mod.rs`
- `syntax.rs`
- `lexer.rs` (if needed)
- `parser.rs`
- `checker.rs`

In `mod.rs`, export your game entry type (for example, `pub use checker::EvalML1Game;`).

## 3. Implement the game entry

In `checker.rs`:

- Define a game type implementing `core::Game`.
- Return `GameKind::<NewVariant>` from `kind()`.
- Parse source text and run rule validation in `check(&self, source: &str)`.
- Return `CheckReport` on success.
- Return `CheckError` with:
  - `CheckErrorKind::Parse` for syntax problems
  - `CheckErrorKind::RuleViolation` for derivation/rule mismatches
  - `CheckErrorKind::Internal` for unexpected internal failures

If available, attach `SourceSpan` to checker-side violations.

## 4. Register dispatch

Edit `src/games/mod.rs`:

- Add `pub mod <new_game>;`
- Add a `run_checker` match arm:
  - `GameKind::<NewVariant> => <new_game>::<GameType>.check(source),`

## 5. Add tests

At minimum:

- Parser tests for valid and invalid inputs.
- Checker tests for:
  - valid derivations
  - wrong rule application
  - unknown rule names (if the parser accepts raw rule text)
- CLI/game routing tests:
  - parsing `--game <new_game>`
  - `run` path reaches the new game checker

Reuse fixture-based tests as done for `Nat`.

## 6. Sync docs

Update these files in the same change set:

- `README.md` (if user-facing usage or status changed)
- `docs/design.md` (module/design snapshot)
- `docs/PLAN.md` (progress and milestones)
- `docs/adr/` (when a new architecture/process/CLI decision is made)

## 7. Run validation gates

Before commit:

```sh
cargo fmt
cargo test
cargo clippy --all-targets --all-features -- -D warnings
```

