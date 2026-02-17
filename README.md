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
- `prover` CLI routing is available, but proof generation is not implemented yet (it currently returns an error).

Examples:

```sh
cargo run -- checker --game Nat copl/001.copl
cat copl/001.copl | cargo run -- checker --game Nat
cargo run -- checker --game Nat -- -input.copl
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
