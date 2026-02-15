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
- Return `CheckReport` on success, with summary as inferred root judgment text.
- Return `CheckError` with:
  - `CheckErrorKind::Parse` for syntax problems
  - `CheckErrorKind::RuleViolation` for derivation/rule mismatches
  - `CheckErrorKind::Internal` for unexpected internal failures
- For `RuleViolation` diagnostics, include actionable hints (`expected` / `actual` / `fix`) when possible.

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

For fixture-based success tests, make the fixture selection and expected-output derivation explicit:

1. Find fixture files for the target game from `copl/` by the game header:
   - `rg 'copl-game: "<GameName>"' copl/*.copl`
2. Use every matched fixture as a success case.
3. For each fixture, assert `CheckReport.summary` equals the fixture root judgment text (the judgment before `by` on the top derivation line), not just a partial substring.

## 6. Sync docs

Update these files in the same change set:

- `README.md` (if user-facing usage or status changed)
- `docs/design.md` (module/design snapshot)
- `docs/PLAN.md` (current progress and priorities)
- `docs/plans/` (archive plan history when a plan is completed/frozen)
- `docs/adr/` (when a new architecture/process/CLI decision is made)

## 7. Run validation gates

Before commit:

```sh
cargo fmt
cargo test
cargo clippy --all-targets --all-features -- -D warnings
```
