# ADR-0003: Use a Typed State Machine (Typestate Style) for Checker CLI Parsing

- Status: Accepted
- Date: 2026-02-07

## Context

A procedural parser (`while` with index mutation) made control flow harder to track.
The project prioritizes declarative design and expressing constraints in types where practical.

## Decision

Implement checker argument parsing as a typed state machine.

- Use typestate-like markers for parser mode and expectation.
- Classify option-mode tokens before applying transitions.
- Process arguments with `try_fold` over tokens.
- Keep behavior compatibility with the existing checker command contract.

## Consequences

### Positive

- State transitions are explicit and easier to review.
- Invalid transitions are harder to encode by accident.
- Better alignment with "specification in types" design preference.

### Negative

- More types and boilerplate than procedural parsing.
- Future contributors need to understand typestate-style patterns.

