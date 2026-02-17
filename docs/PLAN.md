# PLAN

最終更新日: 2026-02-17
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

- [ ] `01` [P1][Decision] prover の CLI 契約を確定する（`checker` 互換で `--game <name> [file]` / `stdin` / `--` を採用するかを明文化し、必要なら ADR を更新）。
- [ ] `02` [P1][Implementation] `prover` サブコマンドの CLI 解析・実行経路を追加する（`src/cli/prover.rs` と `lib::execute` の経路追加）。
- [ ] `03` [P1][Implementation] Nat prover 入力（judgment 単体）パーサを実装する。
- [ ] `04` [P1][Implementation] Nat prover 本体（`P-Zero` / `P-Succ` / `T-Zero` / `T-Succ`）を実装し、導出木 AST を構築する。
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
