# ADR-0005: Parse Nat Derivations as Generic Trees and Validate Rule Arity in Checker

- Status: Accepted
- Date: 2026-02-12

## Context

`copl-rs` is adding Nat derivation checking incrementally.
We confirmed that the reference implementation (`copl-tools`) uses a generic derivation representation (`rule name + list of premises`) and performs most rule-shape validation in generated checker code.

We considered a rule-indexed typed derivation model, but this makes premise arity a parse-time concern.
For CoPL derivation checking, premise-count mismatches are better classified as derivation rule violations than syntax errors.

## Decision

Use a generic derivation tree for Nat parser output and move premise-arity validation to checker.

- Parse Nat input into `judgment + rule + subderivations` (premise-proving child derivations) without enforcing rule-specific premise counts in parser.
- Keep unknown rule names syntactically valid in parser, and report them as `RuleViolation` errors during checker rule resolution.
- Validate rule-specific premise arity and structural constraints in checker.
- Report these violations as `RuleViolation`, not `Parse`.

## Consequences

### Positive

- Parser remains focused on syntax and produces a uniform tree representation.
- Error classification becomes clearer: syntax issues vs derivation-rule issues.
- Rule-validation behavior aligns better with CoPL checker semantics.

### Negative

- Checker must handle more invalid intermediate states explicitly.
- Some invariants move from type-level guarantees to runtime validation.
- Rule-validation tests become more important to avoid regressions.
