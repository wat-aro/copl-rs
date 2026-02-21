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
cargo install --path .
```

## Usage

Command shape:

```sh
copl-rs checker --game <name> [file]
copl-rs prover --game <name> [file]
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
- `prover --game EvalRefML3` accepts a single judgment (`sigma / Gamma |- e evalto v / sigma'`; legacy `Gamma |- e / sigma evalto v / sigma'` is also accepted) and prints a generated derivation in plain text.
- `prover --game TypingML2` currently accepts the same typing-judgment surface as `TypingML4` (`Gamma |- e : t`) and prints a generated derivation in plain text.
- `prover --game TypingML3` currently accepts the same typing-judgment surface as `TypingML4` (`Gamma |- e : t`) and prints a generated derivation in plain text.
- `prover --game TypingML4` accepts a single typing judgment (`Gamma |- e : t`) and prints a generated derivation in plain text.
- `prover --game TypingML5` currently accepts the same typing-judgment surface as `TypingML4` (`Gamma |- e : t`) and prints a generated derivation in plain text.
- `prover --game TypingML6` currently accepts the same typing-judgment surface as `TypingML4` (`Gamma |- e : t`) and prints a generated derivation in plain text.
- `prover --game PolyTypingML3` currently accepts the same typing-judgment surface as `PolyTypingML4` (`Gamma |- e : t`) and prints a generated derivation in plain text.
- `prover --game PolyTypingML4` accepts a single typing judgment (`Gamma |- e : t`) and prints a generated derivation in plain text.
- `prover --game NamelessML3` accepts a single translation judgment (`Gamma |- e ==> e'`) and prints a generated derivation in plain text.
- `prover --game EvalNamelessML3` accepts a single evaluation judgment (`Gamma |- e evalto v`, `... plus ... is ...`, `... minus ... is ...`, `... times ... is ...`, `... less than ... is ...`) and prints a generated derivation in plain text.
- `prover --game EvalNatExp` accepts a single judgment (`... evalto ...`, `... plus ... is ...`, `... times ... is ...`) and prints a generated derivation in plain text.
- `prover --game ReduceNatExp` accepts a single judgment (`... ---> ...`, `... -d-> ...`, `... -*-> ...`, `... plus ... is ...`, `... times ... is ...`) and prints a generated derivation in plain text.
- For non-derivable `prover --game Nat`, `prover --game CompareNat1`, `prover --game CompareNat2`, `prover --game CompareNat3`, `prover --game EvalML1`, `prover --game EvalML1Err`, `prover --game EvalML2`, `prover --game EvalML3`, `prover --game EvalML4`, `prover --game EvalML5`, `prover --game EvalML6`, `prover --game EvalContML1`, `prover --game EvalContML4`, `prover --game EvalRefML3`, `prover --game TypingML2`, `prover --game TypingML3`, `prover --game TypingML4`, `prover --game TypingML5`, `prover --game TypingML6`, `prover --game PolyTypingML3`, `prover --game PolyTypingML4`, `prover --game NamelessML3`, `prover --game EvalNamelessML3`, `prover --game EvalNatExp`, and `prover --game ReduceNatExp` input, plain-text diagnostics include `expected` / `actual` / `fix` where available.

Examples:

```sh
copl-rs checker --game Nat copl/001.copl
cat copl/001.copl | copl-rs checker --game Nat
copl-rs checker --game Nat -- -input.copl
echo "S(S(Z)) times S(Z) is S(S(Z))" | copl-rs prover --game Nat
echo "S(S(Z)) is less than S(S(S(S(S(Z)))))" | copl-rs prover --game CompareNat1
echo "S(S(Z)) is less than S(S(S(S(S(Z)))))" | copl-rs prover --game CompareNat2
echo "S(S(Z)) is less than S(S(S(S(S(Z)))))" | copl-rs prover --game CompareNat3
echo "3 + if -23 < -2 * 8 then 8 else 2 + 4 evalto 11" | copl-rs prover --game EvalML1
echo "1 + true + 2 evalto error" | copl-rs prover --game EvalML1Err
echo "|- let x = 3 * 3 in let y = 4 * x in x + y evalto 45" | copl-rs prover --game EvalML2
echo "|- let rec f = fun x -> x + 1 in f 2 evalto 3" | copl-rs prover --game EvalML3
echo "|- let x = 1 :: [] in match x with [] -> 0 | a :: b -> a evalto 1" | copl-rs prover --game EvalML4
echo "|- match 1 :: [] with [] -> 0 | x :: xs -> x evalto 1" | copl-rs prover --game EvalML5
echo "|- match 1 :: [] with [] -> 0 | x :: xs -> x evalto 1" | copl-rs prover --game EvalML6
echo "3 + 5 evalto 8" | copl-rs prover --game EvalContML1
echo "|- 1 + 2 evalto 3" | copl-rs prover --game EvalContML4
echo "|- let x = ref 1 in let u = x := 2 in !x evalto 2 / @l0 = 2" | copl-rs prover --game EvalRefML3
echo "|- let x = 1 in x + 2 : int" | copl-rs prover --game TypingML2
echo "|- let f = fun x -> x + 1 in f 2 : int" | copl-rs prover --game TypingML3
echo "|- fun x -> x + 1 : int -> int" | copl-rs prover --game TypingML4
echo "|- let rec hd = fun xs -> match xs with [] -> 0 | y :: ys -> y in hd (1 :: 2 :: []) : int" | copl-rs prover --game TypingML5
echo "|- let rec hd = fun xs -> match xs with [] -> 0 | y :: ys -> y in hd (1 :: 2 :: []) : int" | copl-rs prover --game TypingML6
echo "|- let id = fun x -> x in let y = id 1 in id true : bool" | copl-rs prover --game PolyTypingML3
echo "|- fun x -> x : 'a -> 'a" | copl-rs prover --game PolyTypingML4
echo "|- let x = 3 in x ==> let . = 3 in #1" | copl-rs prover --game NamelessML3
echo "|- let . = 3 in #1 evalto 3" | copl-rs prover --game EvalNamelessML3
echo "S(Z) + S(Z) evalto S(S(Z))" | copl-rs prover --game EvalNatExp
echo "S(Z) + S(Z) ---> S(S(Z))" | copl-rs prover --game ReduceNatExp
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
EvalRefML3
TypingML2
TypingML3
TypingML4
TypingML5
TypingML6
PolyTypingML3
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
