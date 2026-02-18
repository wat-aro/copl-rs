# PLAN

最終更新日: 2026-02-18
このファイルは現在計画の単一ソースです。

## 運用ルール

- 現在の計画・進捗・改善バックログはこのファイルだけを更新する。
- 実装スコープ（対象機能・着手順・pending 判断）の正本はこのファイルとし、`AGENTS.md` には重複記載しない。
- 変更時は関連ドキュメント（`README.md` / `docs/design.md` / `AGENTS.md` / ADR）と同一変更セットで同期する。
- タスク実行中に当該タスクのスコープ外の改善を発見した場合は、バックログに追加し、優先度と依存関係を考慮して適切な位置へ挿入する。
- 各タスクの完了メモには、`R1` から `R5` までの `review -> improve` 記録をネストして残す（指摘がない回は「指摘なし」と明記する）。
- レビューで出た指摘は、タスク範囲内なら完了前に必ず修正し、タスク範囲外ならバックログへ追加する。

## 完了メモ書式（テンプレート）

```md
完了メモ（YYYY-MM-DD）:
- 実装:
  - <実装内容>
- テスト:
  - <追加/更新したテスト>
- ドキュメント:
  - <更新したファイル>
- R1:
  - Finding: <指摘内容 or 指摘なし>
  - Action: <修正内容 or なし>
  - Scope: <in-scope / out-of-scope>
  - Backlog: <追加タスクID or なし>
- R2:
  - Finding: ...
  - Action: ...
  - Scope: ...
  - Backlog: ...
- R3:
  - Finding: ...
  - Action: ...
  - Scope: ...
  - Backlog: ...
- R4:
  - Finding: ...
  - Action: ...
  - Scope: ...
  - Backlog: ...
- R5:
  - Finding: ...
  - Action: ...
  - Scope: ...
  - Backlog: ...
- 検証:
  - `cargo fmt`: <pass/fail/skip + 理由>
  - `cargo test`: <pass/fail/skip + 理由>
  - `cargo clippy --all-targets --all-features -- -D warnings`: <pass/fail/skip + 理由>
```

## 現在の計画

### Prover 実装計画（Nat-first）

最終更新日: 2026-02-17
このフェーズのスコープ: `prover` サブコマンドを Nat game 向けに最小実装し、`checker` が受理する導出を生成できる状態にする。

#### 背景

- checker は対象 18 game で実装済み。
- いま必要なのは「導出の検証」ではなく「導出の生成」。
- 調査メモ: `docs/prover-strategy-survey.md`

#### フェーズ1のスコープ

- `copl-rs prover --game Nat [file]` を実装する。
- 入力 judgment から Nat の導出木を生成し、プレーンテキストで出力する。
- 生成結果は `checker` に通ることを保証する。

#### フェーズ1の非スコープ

- Nat 以外の game への prover 展開。
- 汎用探索エンジン（単一化 + バックトラック + tabling）の導入。
- JSON 出力。
- 部分導出（hole 付き）入力。

#### 実装方針

- まずは game 特化の規則駆動再帰（backward-chaining）で実装する。
- Nat の規則構成:
  - `resolve_plus(l, r, out)` は `P-Zero` / `P-Succ` で構成する。
  - `resolve_times(l, r, out)` は `T-Zero` / `T-Succ` で構成し、必要な `plus` 前提を再帰で構築する。
- 出力順と整形を固定し、同一入力で同一導出を返す。
- 将来の共通化は prover 対象 game が 2〜3 件になった時点で再評価する。

#### バックログ（着手優先順）

- 運用ルール:
  - 未完了タスクは上から順に着手する。
  - 優先度は `P1`（最優先）/ `P2`（中優先）/ `P3`（低優先）で表記する。
  - 順序を入れ替える場合は、この節に理由を追記する。
  - タスク実行中に発見したスコープ外改善は、優先度と依存関係を検討したうえでバックログへ追加し、着手順に合う位置へ挿入する。
  - 実装完了後は `AGENTS.md` の Design Principles にある判断基準（高凝集・低結合 / `YAGNI` / `KISS`）で 5 回レビューし、各回の改善内容（または指摘なし）を完了メモに記録する。
  - レビュー指摘は、範囲内なら当該タスク内で修正し、範囲外ならバックログに追加して優先度順へ挿入する。

- [x] `01` [P1][Decision] prover の CLI 契約を確定する（`checker` 互換で `--game <name> [file]` / `stdin` / `--` を採用するかを明文化し、必要なら ADR を更新）。  
  完了メモ（2026-02-17）:
  - 実装:
    - `prover` の CLI 契約を `checker` 互換（`--game <name> [file]` / `stdin` / `--` / case-insensitive game name）で確定した。
  - テスト:
    - 追加/更新なし（Decision タスクのためコード変更なし）。
  - ドキュメント:
    - `docs/adr/0002-subcommand-cli-and-unified-game-option.md` を更新し、`prover` の契約を planned から確定仕様へ更新した。
    - `docs/design.md` を更新し、CLI 互換条件と `prover` 追加方針に確定契約を反映した。
    - `docs/PLAN.md` の当該タスクを完了化した。
  - R1:
    - Finding: `docs/adr/0002` の `prover` 予定コマンドが `<file>` 必須として記載され、`checker` 互換要件と不一致だった。
    - Action: `copl-rs prover --game <name> [file]` に修正し、`stdin` / `--` / case-insensitive を明記した。
    - Scope: in-scope
    - Backlog: なし
  - R2:
    - Finding: `docs/design.md` の互換性節が `checker` のみを対象に書かれていた。
    - Action: 互換性節に `prover` 契約（実装時適用）を追記した。
    - Scope: in-scope
    - Backlog: なし
  - R3:
    - Finding: 指摘なし
    - Action: なし
    - Scope: in-scope
    - Backlog: なし
  - R4:
    - Finding: 指摘なし
    - Action: なし
    - Scope: in-scope
    - Backlog: なし
  - R5:
    - Finding: 指摘なし
    - Action: なし
    - Scope: in-scope
    - Backlog: なし
  - 検証:
    - `cargo fmt`: pass
    - `cargo test`: pass
    - `cargo clippy --all-targets --all-features -- -D warnings`: pass
- [x] `02` [P1][Implementation] `prover` サブコマンドの CLI 解析・実行経路を追加する（`src/cli/prover.rs` と `lib::execute` の経路追加）。  
  完了メモ（2026-02-17）:
  - 実装:
    - `Command::Prover(ProverCommand)` を追加し、`Cli::parse` で `prover` サブコマンドを受理するようにした。
    - `src/cli/prover.rs` を追加し、`prover --game <name> [file]` を解析可能にした。
    - `src/cli/game_command.rs` を追加し、`checker` / `prover` 共通のゲーム選択・入力元解析（`stdin` / `--` 含む）を typed-state parser で共通化した。
    - `lib::execute` に `Command::Prover` 分岐を追加し、現時点では `RunError::ProverNotImplemented` を返す経路を追加した。
  - テスト:
    - `cli::tests::parses_prover_with_stdin`
    - `cli::tests::parses_prover_with_file`
    - `cli::tests::parses_prover_dash_prefixed_file_after_double_dash`
    - `tests::routes_prover_nat_to_not_implemented_error`
  - ドキュメント:
    - `README.md` に `prover --game <name> [file]` のコマンド形と現状（未実装エラー）を追記した。
    - `docs/design.md` を更新し、`prover` の CLI/実行経路追加と現在状態を反映した。
    - `docs/PLAN.md` の当該タスクを完了化した。
  - R1:
    - Finding: 既存 CLI テストが `Command::Checker` 単一バリアント前提で、`Command::Prover` 追加時にパターンが崩れる。
    - Action: `let ... else` で明示的に checker/prover を期待する形に更新した。
    - Scope: in-scope
    - Backlog: なし
  - R2:
    - Finding: `prover` が `checker` パーサ実装へ直接依存するとサブコマンド間結合が強くなる。
    - Action: `src/cli/game_command.rs` を導入し、共通パーサを切り出して `checker` / `prover` を薄い adapter に分離した。
    - Scope: in-scope
    - Backlog: なし
  - R3:
    - Finding: CLI エラー usage が `checker` だけを示していた。
    - Action: usage 表示を `checker` / `prover` の両方を示す形式に更新した。
    - Scope: in-scope
    - Backlog: なし
  - R4:
    - Finding: `prover` 経路追加後のユーザー向け説明が README/design と不一致だった。
    - Action: `README.md` と `docs/design.md` を同一変更で更新した。
    - Scope: in-scope
    - Backlog: なし
  - R5:
    - Finding: 指摘なし
    - Action: なし
    - Scope: in-scope
    - Backlog: なし
  - 検証:
    - `cargo fmt`: pass
    - `cargo test`: pass
    - `cargo clippy --all-targets --all-features -- -D warnings`: pass
- [x] `03` [P1][Implementation] Nat prover 入力（judgment 単体）パーサを実装する。  
  完了メモ（2026-02-18）:
  - 実装:
    - `src/games/nat/parser.rs` に judgment 単体入力向け `parse_judgment_source` を追加した。
    - `src/games/nat/mod.rs` に `validate_prover_input` を追加し、Nat prover 入力検証の窓口を用意した。
    - `src/lib.rs` の `prover` 経路で `--game Nat` のときに judgment 入力を先にパースし、成功時は従来どおり未実装エラーへ進むようにした。
  - テスト:
    - `games::nat::parser::tests::parses_judgment_only_input_for_prover`
    - `games::nat::parser::tests::rejects_derivation_input_in_judgment_only_parser`
    - `tests::routes_prover_nat_with_invalid_judgment_to_parse_error`
  - ドキュメント:
    - `README.md` に Nat prover 入力の現状（judgment 単体パース）を追記した。
    - `docs/design.md` に Nat prover 入力パース実装済みの状態を反映した。
    - `docs/PLAN.md` の当該タスクを完了化した。
  - R1:
    - Finding: `parse_judgment_source` 追加直後に未使用警告が出ており、モジュール責務が完結していなかった。
    - Action: `validate_prover_input` と `lib::execute` の Nat prover 経路を追加し、実行経路から利用する形にした。
    - Scope: in-scope
    - Backlog: なし
  - R2:
    - Finding: judgment-only parser が導出入力を誤受理しないことを固定化する回帰テストが不足していた。
    - Action: 導出入力を拒否するテストを追加し、EOF までの厳格パースを検証した。
    - Scope: in-scope
    - Backlog: なし
  - R3:
    - Finding: prover の挙動変更（Nat 入力の事前パース）に対して README/design の説明が古かった。
    - Action: `README.md` と `docs/design.md` を同一変更で更新した。
    - Scope: in-scope
    - Backlog: なし
  - R4:
    - Finding: 指摘なし
    - Action: なし
    - Scope: in-scope
    - Backlog: なし
  - R5:
    - Finding: 指摘なし
    - Action: なし
    - Scope: in-scope
    - Backlog: なし
  - 検証:
    - `cargo fmt`: pass
    - `cargo test`: pass
    - `cargo clippy --all-targets --all-features -- -D warnings`: pass
- [x] `04` [P1][Implementation] Nat prover 本体（`P-Zero` / `P-Succ` / `T-Zero` / `T-Succ`）を実装し、導出木 AST を構築する。  
  完了メモ（2026-02-18）:
  - 実装:
    - `src/games/nat/prover.rs` を追加し、judgment から Nat 導出木 AST を生成する再帰 prover を実装した。
    - `P-Zero` / `P-Succ` / `T-Zero` / `T-Succ` を規則どおりに適用し、`NatDerivation` を構築するようにした。
    - `src/games/nat/mod.rs` に `prove` を追加し、judgment-only parser と prover 本体を接続した。
    - `src/lib.rs` の Nat prover 経路で AST 構築を実行しつつ、現状の外部挙動（未実装エラー）を維持した。
  - テスト:
    - `games::nat::prover::tests::proves_plus_judgment_with_p_zero`
    - `games::nat::prover::tests::proves_times_judgment_with_t_zero`
    - `games::nat::prover::tests::proves_plus_judgment_with_p_succ_chain`
    - `games::nat::prover::tests::proves_times_judgment_with_t_succ_chain`
    - `games::nat::prover::tests::rejects_non_derivable_plus_judgment`
    - `games::nat::prover::tests::rejects_non_derivable_times_judgment`
    - `games::nat::prover::tests::builds_same_derivation_shape_as_fixture_007`
    - `tests::routes_prover_nat_with_non_derivable_judgment_to_not_implemented_error`
  - ドキュメント:
    - `docs/design.md` に Nat prover 本体実装済み・pretty-printer 未実装の現状を反映した。
    - `docs/PLAN.md` の当該タスクを完了化した。
  - R1:
    - Finding: prover 本体追加直後、実行経路未接続だと dead code と責務分断が残る。
    - Action: `nat::prove` を追加し、`lib::execute` の Nat prover 分岐から呼ぶ形に接続した。
    - Scope: in-scope
    - Backlog: なし
  - R2:
    - Finding: 規則実装の妥当性を固定化する回帰テストが、単純な rule-name 断片確認だけでは弱い。
    - Action: `copl/007.copl` の解析結果と生成 AST の形状一致を再帰比較するテストを追加した。
    - Scope: in-scope
    - Backlog: なし
  - R3:
    - Finding: 非導出 judgment 入力時の CLI 振る舞いが暫定仕様であることがコードから読み取りづらい。
    - Action: 現状の互換挙動（not implemented 維持）を固定するテストと注記コメントを追加した。
    - Scope: in-scope
    - Backlog: なし
  - R4:
    - Finding: `src/games/nat/prover.rs` の import が重複行で可読性が下がっていた。
    - Action: `core` import を 1 行に統合した。
    - Scope: in-scope
    - Backlog: なし
  - R5:
    - Finding: 指摘なし
    - Action: なし
    - Scope: in-scope
    - Backlog: なし
  - 検証:
    - `cargo fmt`: pass
    - `cargo test`: pass
    - `cargo clippy --all-targets --all-features -- -D warnings`: pass
- [ ] `05` [P1][Implementation] 導出木の pretty-printer を実装し、checker が受理する形式で出力する。
- [ ] `06` [P1][Test] ゴールデンテストを追加する（`S(S(Z)) times S(Z) is S(S(Z))` の導出が期待形で出力される）。
- [ ] `07` [P1][Test] round-trip 検証を追加する（`prover` 出力を `checker` に渡して成功し、root judgment が一致する）。
- [ ] `08` [P2][Implementation] 不可能 judgment に対するエラーメッセージ方針を定義し実装する（plain text）。
- [ ] `09` [P2][Documentation] 実装完了時に `README.md` / `docs/design.md` / `AGENTS.md` / ADR を同期する。
- [ ] `10` [P3][Improvement] prover 対応 game が 2〜3 件になった時点で、汎用 proof-search コア導入の要否を再評価する。

#### 共通完了条件（Implementation タスク）

- `prover --game Nat` で CLI が受理される。
- 代表入力で期待導出を出力できる。
- 生成導出の checker round-trip テストが通る。
- 代表的な失敗系（導出不能 judgment）が通る。
- `R1` から `R5` の `review -> improve` 記録が完了している（指摘なしの回は「指摘なし」を明記）。
- レビュー指摘は、範囲内は修正済み、範囲外はバックログ追加済みである。
- 変更に合わせて `README.md` / `docs/design.md` / `docs/PLAN.md` を同期する。

#### 検証ゲート

コミット前に以下を実行する:

- `cargo fmt`
- `cargo test`
- `cargo clippy --all-targets --all-features -- -D warnings`

## 履歴計画

- 完了または凍結した計画は `docs/plans/` に保存する。
- このファイルには現在の計画のみを保持する。
- 履歴一覧:
  - [2026-02-16 Checker 実装ロードマップ（完了）](plans/2026-02-16-checker-roadmap-completed.md)
  - [2026-02-15 Checker 実装ロードマップ](plans/2026-02-15-checker-roadmap.md)
  - [2026-02-14 Nat Checker 実装計画](plans/2026-02-14-nat-checker.md)
