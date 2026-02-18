# ADR-0009: Split CLI Parser by Subcommand Before Adding Prover

- Status: Accepted
- Date: 2026-02-16 (updated: 2026-02-18)

## Context

At decision time, the CLI supported only `checker`, and `prover` was planned.
Keeping all parser details in one module would increase coupling between subcommands and make future changes harder to review.
ADR-0002 already established subcommand-style CLI, and ADR-0003 established typestate parsing for checker options.
Before implementing `prover`, we need a stable parser boundary strategy.

## Decision

Adopt parser separation by subcommand module.

- Keep `src/cli.rs` as the public CLI domain model and subcommand dispatcher.
- Move checker-specific parsing implementation into `src/cli/checker.rs`.
- Keep checker parsing behavior and compatibility guarantees unchanged.
- Reuse the same pattern for additional subcommands (for example, `src/cli/prover.rs`) instead of extending a single monolithic parser file.

## Consequences

### Positive

- Subcommand parser responsibilities are explicit and localized.
- Adding `prover` was done with smaller, isolated diffs.
- The existing checker typestate parser remains intact without leaking implementation details into other commands.

### Negative

- `CliError` constructors need controlled cross-module visibility (`pub(super)`), which slightly increases internal API surface.
- There is one more module/file boundary to navigate during CLI changes.
