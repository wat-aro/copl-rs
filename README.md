# copl-rs

`copl-rs` is a command-line tool for CoPL derivations.

## Requirements

- Rust toolchain (`cargo`, `rustc`)
- Git (for submodules)

## Setup

```sh
git clone <this-repo>
cd copl-rs
git submodule update --init --recursive
```

## Usage

Command shape:

```sh
cargo run -- checker --game <name> [file]
cargo run -- prover --game <name> [file]
```

- If `[file]` is omitted, input is read from `stdin`.
- Game names are case-insensitive (`Nat` and `nat` are both accepted).
- Use `--` before a file name that starts with `-`.
- `prover --game Nat` accepts a single judgment (`... plus ... is ...` or `... times ... is ...`) and prints a generated derivation in plain text.
- `prover --game CompareNat1` accepts a single judgment (`... is less than ...`) and prints a generated derivation in plain text.
- `prover --game CompareNat2` accepts a single judgment (`... is less than ...`) and prints a generated derivation in plain text.
- `prover --game CompareNat3` accepts a single judgment (`... is less than ...`) and prints a generated derivation in plain text.
- `prover --game EvalML1` accepts a single judgment (`... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`) and prints a generated derivation in plain text.
- `prover --game EvalML3` accepts a single judgment (`Gamma |- ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`) and prints a generated derivation in plain text.
- For non-derivable `prover --game Nat`, `prover --game CompareNat1`, `prover --game CompareNat2`, `prover --game CompareNat3`, `prover --game EvalML1`, and `prover --game EvalML3` input, plain-text diagnostics include `expected` / `actual` / `fix` where available.
- `prover` for games other than `Nat`, `CompareNat1`, `CompareNat2`, `CompareNat3`, `EvalML1`, and `EvalML3` is not implemented yet.

Examples:

```sh
cargo run -- checker --game Nat copl/001.copl
cat copl/001.copl | cargo run -- checker --game Nat
cargo run -- checker --game Nat -- -input.copl
echo "S(S(Z)) times S(Z) is S(S(Z))" | cargo run -- prover --game Nat
echo "S(S(Z)) is less than S(S(S(S(S(Z)))))" | cargo run -- prover --game CompareNat1
echo "S(S(Z)) is less than S(S(S(S(S(Z)))))" | cargo run -- prover --game CompareNat2
echo "S(S(Z)) is less than S(S(S(S(S(Z)))))" | cargo run -- prover --game CompareNat3
echo "3 + if -23 < -2 * 8 then 8 else 2 + 4 evalto 11" | cargo run -- prover --game EvalML1
echo "|- let rec f = fun x -> x + 1 in f 2 evalto 3" | cargo run -- prover --game EvalML3
```

Supported `--game` values:

```text
Nat
CompareNat1
CompareNat2
CompareNat3
EvalML1
EvalML1Err
EvalML2
EvalML3
EvalML4
EvalML5
EvalContML1
EvalContML4
TypingML4
PolyTypingML4
NamelessML3
EvalNamelessML3
EvalNatExp
ReduceNatExp
```

On success, the checker prints the inferred root judgment text in plain text, for example:

```text
Z plus Z is Z
```

## Contributing

Development workflow, validation commands, and repository policies are documented in
`CONTRIBUTING.md`.
