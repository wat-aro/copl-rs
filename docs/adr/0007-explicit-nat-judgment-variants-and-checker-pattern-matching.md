# ADR-0007: Model `NatJudgment` as Explicit Variants and Validate Premises in Checker

- Status: Accepted
- Date: 2026-02-13

## Context

ADR-0005 established a generic Nat derivation tree (`judgment + rule name + subderivations`) and moved premise arity validation to checker.
We still needed to choose how to model Nat judgments and organize checker rule validation:

- Keep a shared judgment shape (for example, common `left/right/result + operator`) and branch by operator.
- Use explicit judgment variants that encode judgment form directly in the type.
- Add rule-indexed typed derivation nodes in parser output.

Checker logic should also stay close to upstream CoPL checker behavior, where each rule validates conclusion form, premise count, and premise form in declarative order.

## Decision

Adopt explicit Nat judgment variants and keep rule application validation in checker-side pattern matching.

- Represent Nat judgments as explicit enum variants:
  - `PlusIs { left, right, result }`
  - `TimesIs { left, right, result }`
- Keep parser output as the generic derivation tree from ADR-0005.
- Do not introduce rule-indexed parser nodes.
- In checker, validate each rule by pattern matching:
  - conclusion form
  - `subderivations` shape/count
  - inferred premise judgment forms and term links

## Consequences

### Positive

- Judgment form is explicit in the type, reducing ambiguity in checker branches.
- Rule functions can be written in a declarative style closer to upstream CoPL checker structure.
- Parser/checker responsibility split remains consistent with ADR-0005.

### Negative

- Premise shape validity remains a runtime checker concern, not a parse-time type guarantee.
- Checker code still needs explicit per-rule matching and term-link comparisons.
- Adding new judgment forms requires updating parser construction and checker match branches.
