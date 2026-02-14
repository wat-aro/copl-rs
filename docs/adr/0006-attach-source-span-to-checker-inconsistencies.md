# ADR-0006: Attach SourceSpan to Checker Inconsistency Errors

- Status: Accepted
- Date: 2026-02-12

## Context

ADR-0005 established that Nat rule-shape mismatches and unknown rule names are checker-level `RuleViolation` errors, not parse errors.
After implementing checker-side validation, diagnostics still needed a stable way to identify where the failure occurred in source text.

Without location data on checker inconsistencies, users cannot reliably map a rule-validation failure to the exact derivation node in the input.

## Decision

Require checker inconsistencies to carry source location (`SourceSpan`) of the failing derivation node.

- Parse and keep derivation node spans in the generic Nat derivation tree.
- When checker validation fails, return errors with the corresponding node span.
- If checker code returns an error without span, attach the current derivation node span before propagating.
- Keep error kind classification unchanged (`RuleViolation` remains checker-level).
- For checker-level `RuleViolation` diagnostics, include actionable hints (`expected` / `actual` / `fix`) where practical.

## Consequences

### Positive

- Rule-validation failures can be traced directly to `line:column` in user input.
- Diagnostics become actionable without changing the CLI contract.
- Future checker rules inherit consistent location behavior by default.
- Users can understand concrete next edits from a single error message.

### Negative

- Checker flow now has an additional responsibility: preserving/attaching spans on propagation.
- Tests must verify both error classification and span presence to avoid regressions.
