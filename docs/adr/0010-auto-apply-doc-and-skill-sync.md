# ADR-0010: Auto-Apply Documentation and Skill Sync

- Status: Accepted
- Date: 2026-02-15

## Context

`copl-rs` requires documentation synchronization when implementation changes architecture, CLI behavior, or operational policy.
ADR-0009 introduced a confirm-before-edit step for docs/skills updates.
In practice, this added waiting overhead even when updates were straightforward and already implied by implemented changes.

## Decision

Adopt auto-apply synchronization for documentation and skill updates:

1. When implementation outcomes imply required documentation/skill updates, apply them in the same change set by default.
2. Report updated files and rationale after applying changes.
3. Ask for explicit confirmation only when scope is ambiguous, potentially destructive, or not clearly required by the implemented changes.
4. Update operational docs and relevant skills to reflect this default behavior.

ADR-0009 is superseded by this decision.

## Consequences

### Positive

- Reduces turnaround time for routine documentation synchronization.
- Keeps implementation and documentation drift smaller by default.
- Maintains traceability by reporting exactly what was updated.

### Negative

- Requires stricter discipline to keep update scope minimal.
- Increases the importance of clear post-update reporting to avoid surprise edits.
