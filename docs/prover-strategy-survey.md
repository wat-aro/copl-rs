# Prover Strategy Survey (Nat-first)

- Date: 2026-02-16
- Status: Research memo (no architecture decision yet)

## 1. Goal

`prover` should accept a judgment and output a derivation tree that `checker` accepts.

Nat example (target behavior):

Input:

```text
S(S(Z)) times S(Z) is S(S(Z))
```

Expected output shape:

```text
S(S(Z)) times S(Z) is S(S(Z)) by T-Succ {
  S(Z) times S(Z) is S(Z) by T-Succ {
    Z times S(Z) is Z by T-Zero {};
    S(Z) plus Z is S(Z) by P-Succ {
      Z plus Z is Z by P-Zero {}
    }
  };
  S(Z) plus S(Z) is S(S(Z)) by P-Succ {
    Z plus S(Z) is S(Z) by P-Zero {}
  }
}
```

This is a proof-search problem: find a derivation `D` such that `checker(D)` succeeds and the inferred root judgment equals the input judgment.

## 2. Evaluation Criteria

- Soundness: every prover output must pass `checker`.
- Completeness (scope-bounded): for supported games/modes, find a proof if one exists.
- Termination: avoid infinite search.
- Determinism of output: stable canonical proof for the same input.
- Implementation cost: start small (YAGNI/KISS), then generalize.
- Extensibility: can be reused for future games without full rewrite.

## 3. Candidate Approaches

### A. Rule-directed recursive synthesis (game-specific backward chaining)

Model each rule directly in Rust as a constructor from conclusion to premises.

- For Nat `plus is`: direct structural recursion on the left operand (`P-Zero`/`P-Succ`).
- For Nat `times is`: recurse by `T-Zero`/`T-Succ`, then build required `plus is` premise.

Pros:

- Lowest implementation risk.
- Very predictable output and error messages.
- Easy to make output canonical.

Cons:

- Per-game custom code.
- Generalization to richer/non-deterministic systems requires redesign.

### B. Generic backward chaining + first-order unification + backtracking

Represent each inference rule as a schema with meta-variables.
Given goal `G`, unify with each rule conclusion, generate subgoals, recurse.

Pros:

- One engine for many games.
- Natural fit to inference-rule systems.

Cons:

- Requires unification + substitution plumbing + search control.
- Needs termination strategy (depth bound, loop checks, tabling).

Useful optimization: focused proof search to reduce useless branching.

### C. Constraint-based proof search (CLP style)

Treat rule application as generating constraints over unknowns, then solve constraints incrementally.

Pros:

- Good for systems with many unknown intermediates.
- Can encode richer side conditions.

Cons:

- Higher complexity than current project needs.
- Introduces prover architecture decisions early.

### D. Logic-programming style engine (Horn clauses, optional tabling)

Encode rules as relations, run goal-directed search (Prolog style), use tabling where recursion causes repeated subgoals.

Pros:

- Mature conceptual model for proof search.
- Tabling can improve termination/performance for recursive relations.

Cons:

- Engine integration overhead (internal or external).
- Need explicit policy for proof ordering/canonical output.

## 4. Recommendation for `copl-rs`

### Phase 1 (now): Nat-specific prover (Approach A)

Implement a minimal Nat prover with structural recursion, no generic engine.

Why:

- Matches current repository policy (`YAGNI -> KISS -> SOLID`).
- Nat rules are small and mostly deterministic.
- Fastest path to ship a useful `prover` command.

### Phase 2 (later): extract proof-search core when 2-3 prover games exist

After Nat + at least one richer game, refactor shared parts:

- rule schema representation,
- substitution/unification utilities,
- search strategy and trace,
- memoization/tabling hooks.

This avoids speculative abstraction before requirements are concrete.

## 5. Nat-first Concrete Algorithm

Assume ground Nat terms (`Z`, `S(...)`) in input.

### `resolve_plus(l, r, out)`

1. If `l = Z`, succeed iff `out = r`; emit `P-Zero {}`.
2. If `l = S(l1)` and `out = S(o1)`, recursively resolve `l1 plus r is o1`; emit `P-Succ`.
3. Otherwise fail.

### `resolve_times(l, r, out)`

1. If `l = Z`, succeed iff `out = Z`; emit `T-Zero {}`.
2. If `l = S(l1)`, recursively resolve witness `l1 times r is mid`.
3. Resolve `r plus mid is out`.
4. Emit `T-Succ` from the two subderivations.

Notes:

- In this mode, witness `mid` is produced by recursion, so no global search is needed.
- Output is canonical if recursion order and pretty-printer order are fixed.

## 6. Validation Plan

- Golden test: the exact Nat example above.
- Round-trip property: `prover(goal)` output must pass `checker` and print same root judgment.
- Negative tests: impossible goals (for example, `Z times S(Z) is S(Z)`) must fail with plain-text diagnostics.
- CLI compatibility checks for `prover --game Nat [file]`, stdin behavior, and `--` delimiter rule.

## 7. Open Questions (for decision)

- Input format for prover:
  - judgment-only text (simplest), or
  - partial derivation with holes (more expressive).
- Output canonicalization policy if multiple valid derivations exist in future games.
- When to introduce a generic search engine (trigger: second/third prover game).

## 8. References

1. CoPL Nat rules (official): https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/Nat.html
2. Dale Miller et al., "Uniform Proofs as a Foundation for Logic Programming" (1991): https://doi.org/10.1016/0743-1066(91)90034-M
3. Jean-Marc Andreoli, "Logic Programming with Focusing Proofs in Linear Logic" (1992): https://doi.org/10.1093/logcom/2.3.297
4. J. A. Robinson, "A Machine-Oriented Logic Based on the Resolution Principle" (1965): https://doi.org/10.1145/321250.321253
5. Joxan Jaffar and Jean-Louis Lassez, "Constraint Logic Programming" (POPL 1987): https://doi.org/10.1145/41625.41635
6. Luis Damas and Robin Milner, "Principal type-schemes for functional programs" (1982): https://doi.org/10.1145/582153.582176
7. Jana Dunfield and Neel Krishnaswami, "Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism" (2013): https://arxiv.org/abs/1306.6032
8. Weidong Chen and David S. Warren, "Tabled Evaluation With Delaying for General Logic Programs" (1996): https://doi.org/10.1145/227595.227597
9. Frank Pfenning, lecture notes on logic programming proof search strategies (backward/forward chaining): https://www.cs.cmu.edu/~fp/courses/15317-s23/lectures/18-lp.pdf
