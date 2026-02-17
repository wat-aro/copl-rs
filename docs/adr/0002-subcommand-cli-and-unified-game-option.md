# ADR-0002: Adopt Subcommand CLI with Unified `--game` Option

- Status: Accepted
- Date: 2026-02-07 (updated: 2026-02-17)

## Context

The project includes multiple tools (`checker`, and `prover` in the current plan) and multiple CoPL games.
A consistent command shape is needed to reduce user confusion and avoid per-command option drift.
As prover implementation starts, its CLI compatibility contract must be fixed before code changes.

## Decision

Use the binary name `copl-rs` and subcommand-style CLI.

- Command shapes:
  - `copl-rs checker --game <name> [file]`
  - `copl-rs prover --game <name> [file]`
- Keep `--game <name>` as the unified game selector across commands.
- Keep compatibility rules aligned between `checker` and `prover`:
  - Omitted `[file]` means `stdin`.
  - Support `--` delimiter for file names starting with `-`.
  - Keep game-name parsing case-insensitive.

## Consequences

### Positive

- Consistent UX across commands.
- Cleaner command discovery as the toolset grows.
- New games can be added mainly through `GameKind` and dispatch updates.

### Negative

- CLI parsing code needs careful maintenance as subcommands increase.
- We may later split parser implementation by subcommand module to keep files small.
