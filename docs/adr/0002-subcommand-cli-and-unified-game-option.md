# ADR-0002: Adopt Subcommand CLI with Unified `--game` Option

- Status: Accepted
- Date: 2026-02-07

## Context

The project will include multiple tools (`checker` now, `prover` later) and multiple CoPL games.
A consistent command shape is needed to reduce user confusion and avoid per-command option drift.

## Decision

Use the binary name `copl-rs` and subcommand-style CLI.

- Current command:
  - `copl-rs checker --game <name> [file]`
- Planned command shape:
  - `copl-rs prover --game <name> <file>`
- Keep `--game <name>` as the unified game selector across commands.
- Keep compatibility rules for checker:
  - Omitted `[file]` means `stdin`.
  - Support `--` delimiter for file names starting with `-`.

## Consequences

### Positive

- Consistent UX across commands.
- Cleaner command discovery as the toolset grows.
- New games can be added mainly through `GameKind` and dispatch updates.

### Negative

- CLI parsing code needs careful maintenance as subcommands increase.
- We may later split parser implementation by subcommand module to keep files small.

