# ADR-0008: Align Checker Success Output with the Reference Implementation

- Status: Accepted
- Date: 2026-02-14

## Context

`copl-rs checker --game nat` previously returned an internal-oriented success summary (for example, `parsed root rule ...` with a game prefix).
This differed from the reference implementation (`copl-tools`), which prints the inferred root judgment text directly on success.

For side-by-side comparison and operational compatibility, successful output should follow the reference implementation behavior.

## Decision

On successful checking, output the inferred root judgment text in plain text, without game prefix metadata.

- Nat checker success summary is the inferred root judgment string.
- `CheckReport` display prints only the summary text.
- Error classification remains unchanged.

## Consequences

### Positive

- `copl-rs` success output matches reference implementation behavior for Nat checks.
- Users can compare outputs between tools directly.
- Success output becomes the semantically meaningful result, not an implementation detail.

### Negative

- Success output no longer includes the game name prefix.
- Tooling that depended on the old summary text must update.
