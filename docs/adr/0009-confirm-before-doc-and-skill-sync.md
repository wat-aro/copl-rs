# ADR-0009: Confirm-Before-Edit Retrospective for Docs and Skills

- Status: Accepted
- Date: 2026-02-14

## Context

`copl-rs` already requires documentation synchronization when architecture, CLI behavior, or operational policy changes.
In practice, many updates are discovered during implementation sessions and not always reflected consistently.
This also applies to Codex skills, because workflow policy may need changes in both repository docs and skill instructions.

## Decision

Adopt a session retrospective workflow, powered by a `retrospect` skill, with the following rules:

1. Collect documentation and skill update candidates implied by session outcomes.
2. Present candidates to the user and ask for confirmation before editing.
3. Apply only approved updates, using minimal diffs.
4. Include optional proposals for improving existing skills or adding new skills when repeated patterns are observed.

Explicit trigger phrases `retrospect` and `$retrospect` are treated as direct requests to run this workflow.

## Consequences

### Positive

- Reduces drift between implementation decisions and repository documentation.
- Makes updates to skills visible and reviewable in the same workflow as document updates.
- Avoids accidental or premature edits by requiring confirmation before modification.

### Negative

- Adds an explicit confirmation step, which may slightly slow down fast iterations.
- Requires maintaining the `retrospect` skill definition as workflow expectations evolve.
