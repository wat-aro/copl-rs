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
- `prover --game EvalML1Err` accepts a single judgment (`... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`; values include `error`) and prints a generated derivation in plain text.
- `prover --game EvalML2` accepts a single judgment (`Gamma |- ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`) and prints a generated derivation in plain text.
- `prover --game EvalML3` accepts a single judgment (`Gamma |- ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`) and prints a generated derivation in plain text.
- `prover --game EvalML4` accepts a single judgment (`Gamma |- ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`) and prints a generated derivation in plain text.
- `prover --game EvalML5` accepts a single judgment (`Gamma |- ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`, `p matches v when (...)`, `p doesn't match v`) and prints a generated derivation in plain text.
- `prover --game EvalML6` currently accepts the same judgment surface as `EvalML5` (`Gamma |- ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`, `p matches v when (...)`, `p doesn't match v`) and prints a generated derivation in plain text.
- `prover --game EvalContML1` accepts a single judgment (`... evalto ...`, `... >> ... evalto ...`, `... => ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`) and prints a generated derivation in plain text.
- `prover --game EvalContML4` accepts a single judgment (`Gamma |- ... evalto ...`, `... >> ... evalto ...`, `... => ... evalto ...`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`) and prints a generated derivation in plain text.
- `prover --game TypingML4` accepts a single typing judgment (`Gamma |- e : t`) and prints a generated derivation in plain text.
- `prover --game PolyTypingML4` accepts a single typing judgment (`Gamma |- e : t`) and prints a generated derivation in plain text.
- `prover --game NamelessML3` accepts a single translation judgment (`Gamma |- e ==> e'`) and prints a generated derivation in plain text.
- `prover --game EvalNamelessML3` accepts a single evaluation judgment (`Gamma |- e evalto v`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`) and prints a generated derivation in plain text.
- `prover --game EvalNatExp` accepts a single judgment (`... evalto ...`, `... plus ... is ...`, `... times ... is ...`) and prints a generated derivation in plain text.
- `prover --game ReduceNatExp` accepts a single judgment (`... ---> ...`, `... -d-> ...`, `... -*-> ...`, `... plus ... is ...`, `... times ... is ...`) and prints a generated derivation in plain text.
- For non-derivable `prover --game Nat`, `prover --game CompareNat1`, `prover --game CompareNat2`, `prover --game CompareNat3`, `prover --game EvalML1`, `prover --game EvalML1Err`, `prover --game EvalML2`, `prover --game EvalML3`, `prover --game EvalML4`, `prover --game EvalML5`, `prover --game EvalML6`, `prover --game EvalContML1`, `prover --game EvalContML4`, `prover --game TypingML4`, `prover --game PolyTypingML4`, `prover --game NamelessML3`, `prover --game EvalNamelessML3`, `prover --game EvalNatExp`, and `prover --game ReduceNatExp` input, plain-text diagnostics include `expected` / `actual` / `fix` where available.

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
echo "1 + true + 2 evalto error" | cargo run -- prover --game EvalML1Err
echo "|- let x = 3 * 3 in let y = 4 * x in x + y evalto 45" | cargo run -- prover --game EvalML2
echo "|- let rec f = fun x -> x + 1 in f 2 evalto 3" | cargo run -- prover --game EvalML3
echo "|- let x = 1 :: [] in match x with [] -> 0 | a :: b -> a evalto 1" | cargo run -- prover --game EvalML4
echo "|- match 1 :: [] with [] -> 0 | x :: xs -> x evalto 1" | cargo run -- prover --game EvalML5
echo "|- match 1 :: [] with [] -> 0 | x :: xs -> x evalto 1" | cargo run -- prover --game EvalML6
echo "3 + 5 evalto 8" | cargo run -- prover --game EvalContML1
echo "|- 1 + 2 evalto 3" | cargo run -- prover --game EvalContML4
echo "|- fun x -> x + 1 : int -> int" | cargo run -- prover --game TypingML4
echo "|- fun x -> x : 'a -> 'a" | cargo run -- prover --game PolyTypingML4
echo "|- let x = 3 in x ==> let . = 3 in #1" | cargo run -- prover --game NamelessML3
echo "|- let . = 3 in #1 evalto 3" | cargo run -- prover --game EvalNamelessML3
echo "S(Z) + S(Z) evalto S(S(Z))" | cargo run -- prover --game EvalNatExp
echo "S(Z) + S(Z) ---> S(S(Z))" | cargo run -- prover --game ReduceNatExp
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
EvalML6
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
