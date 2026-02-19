# PLAN

最終更新日: 2026-02-19
このファイルは現在計画の単一ソースです。

## 運用ルール

- 現在の計画・進捗・改善バックログはこのファイルだけを更新する。
- 実装スコープ（対象機能・着手順・pending 判断）の正本はこのファイルとし、`AGENTS.md` には重複記載しない。
- 変更時は関連ドキュメント（`README.md` / `docs/design.md` / `AGENTS.md` / ADR）と同一変更セットで同期する。
- タスク実行中に当該タスクのスコープ外の改善を発見した場合は、バックログに追加し、優先度と依存関係を考慮して適切な位置へ挿入する。
- 各タスクの完了メモには、`R1` から `R5` までの `review -> improve` 記録をネストして残す（指摘がない回は「指摘なし」と明記する）。
- レビューで出た指摘は、タスク範囲内なら完了前に必ず修正し、タスク範囲外ならバックログへ追加する。
- バックログタスクを完了（`[ ] -> [x]`）するときは `task-commit` skill を使い、タスクチェックとコミット作成を同一フローで行う。

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

最終更新日: 2026-02-19
このフェーズのスコープ: `prover` サブコマンドを Nat / EvalML1 / EvalML3 向けに実装し、`checker` が受理する導出を生成できる状態にする。

#### 背景

- checker は対象 18 game で実装済み。
- いま必要なのは「導出の検証」ではなく「導出の生成」。
- 調査メモ: `docs/prover-strategy-survey.md`

#### フェーズ1のスコープ

- `copl-rs prover --game Nat [file]` を実装する。
- `copl-rs prover --game EvalML1 [file]` を実装する。
- `copl-rs prover --game EvalML3 [file]` を実装する。
- 入力 judgment から Nat / EvalML1 / EvalML3 の導出木を生成し、プレーンテキストで出力する。
- 生成結果は `checker` に通ることを保証する。

#### フェーズ1の非スコープ

- Nat / EvalML1 / EvalML3 以外の game への prover 展開。
- 汎用探索エンジン（単一化 + バックトラック + tabling）の導入。
- JSON 出力。
- 部分導出（hole 付き）入力。

#### 実装方針

- まずは game 特化の規則駆動再帰（backward-chaining）で実装する。
- Nat の規則構成:
  - `resolve_plus(l, r, out)` は `P-Zero` / `P-Succ` で構成する。
  - `resolve_times(l, r, out)` は `T-Zero` / `T-Succ` で構成し、必要な `plus` 前提を再帰で構築する。
- EvalML1 / EvalML3 は checker の規則順に対応する game 特化 evaluator を実装し、`E-*` / `B-*` 規則の導出を決定的に構成する。
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

- [x] `01`-`17` [Archive] 完了済みタスクをアーカイブへ移動した。
  - 参照: `docs/plans/2026-02-19-prover-roadmap-completed-01-17.md`
- [x] `18` [P2][Implementation] `EvalML4` prover を実装する（judgment-only parser / prover 本体 / pretty-printer / CLI 経路 / round-trip テスト）。
  - 完了メモ（2026-02-19）:
    - 実装:
      - `src/games/eval_ml4/prover.rs` を追加し、`E-*` / `B-*` 規則に対応する決定的な prover を実装した。
      - `src/games/eval_ml4/parser.rs` に judgment-only parser (`parse_judgment_source`) を追加した。
      - `src/games/eval_ml4/syntax.rs` に導出 pretty-printer (`Display for EvalML4Derivation`) を追加した。
      - `src/games/eval_ml4/mod.rs` と `src/lib.rs` に `prover --game EvalML4` の CLI 経路を接続した。
    - テスト:
      - `src/games/eval_ml4/parser.rs` に judgment-only parser の正常系/異常系テストを追加した。
      - `src/games/eval_ml4/syntax.rs` に pretty-printer の leaf/nested テストを追加した。
      - `src/games/eval_ml4/prover.rs` に正常系/異常系/fixture 形状比較テストを追加した。
      - `src/lib.rs` に `prover --game EvalML4` の正常系/異常系/round-trip テストを追加した。
    - ドキュメント:
      - `README.md` / `docs/design.md` / `AGENTS.md` / `docs/PLAN.md` を同期した。
    - R1:
      - Finding: `E-Var` の最近束縛優先を直接検証するテストが不足していた。
      - Action: `src/games/eval_ml4/prover.rs` に `proves_var_with_nearest_binding_by_e_var` を追加した。
      - Scope: in-scope
      - Backlog: なし
    - R2:
      - Finding: 指摘なし
      - Action: なし
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
- [x] `19` [P2][Implementation] `EvalML5` prover を実装する（judgment-only parser / prover 本体 / pretty-printer / CLI 経路 / round-trip テスト）。
  - 完了メモ（2026-02-19）:
    - 実装:
      - `src/games/eval_ml5/prover.rs` を追加し、`E-*` / `M-*` / `NM-*` / `B-*` 規則に対応する決定的な prover を実装した。
      - `src/games/eval_ml5/mod.rs` と `src/lib.rs` に `prover --game EvalML5` の CLI 経路を接続した。
      - `src/games/eval_ml5/parser.rs` の judgment-only parser を `prover` 入口から利用する構成に接続した。
      - `src/games/eval_ml5/syntax.rs` の導出 pretty-printer（`Display for EvalML5Derivation`）を `prover` 出力に利用する構成に接続した。
    - テスト:
      - `src/games/eval_ml5/parser.rs` に judgment-only parser の正常系/異常系テストを追加した。
      - `src/games/eval_ml5/syntax.rs` に pretty-printer の leaf/nested テストを追加した。
      - `src/games/eval_ml5/prover.rs` に正常系/異常系/fixture 形状比較テストを追加した。
      - `src/lib.rs` に `prover --game EvalML5` の正常系/異常系/round-trip テストを追加した。
    - ドキュメント:
      - `README.md` / `docs/design.md` / `AGENTS.md` / `docs/PLAN.md` を同期した。
    - R1:
      - Finding: `EvalML5` round-trip テスト入力がテスト時入力上限（1024 bytes）を超える導出を生成し、`checker` 側で `InputTooLarge` になった。
      - Action: `src/lib.rs` の round-trip テスト入力を導出サイズが上限内に収まる同等シナリオへ置き換えた。
      - Scope: in-scope
      - Backlog: なし
    - R2:
      - Finding: `NM-ConsConsL` のテストで、規則集合では導出不能な head mismatch（`[]` vs 整数）を使っていた。
      - Action: `src/games/eval_ml5/prover.rs` のテスト値を list-vs-list mismatch に修正し、`NM-ConsNil` を前提に導出できる形へ変更した。
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
- [x] `20` [P2][Implementation] `EvalContML1` prover を実装する（judgment-only parser / prover 本体 / pretty-printer / CLI 経路 / round-trip テスト）。
  - 完了メモ（2026-02-19）:
    - 実装:
      - `src/games/eval_cont_ml1/prover.rs` を追加し、`E-*` / `C-*` / `B-*` 規則に対応する決定的な prover を実装した。
      - `src/games/eval_cont_ml1/parser.rs` に judgment-only parser (`parse_judgment_source`) を追加した。
      - `src/games/eval_cont_ml1/syntax.rs` に導出 pretty-printer（`Display for EvalContML1Derivation`）を追加した。
      - `src/games/eval_cont_ml1/mod.rs` と `src/lib.rs` に `prover --game EvalContML1` の CLI 経路を接続した。
    - テスト:
      - `src/games/eval_cont_ml1/parser.rs` に judgment-only parser の正常系/異常系テストを追加した。
      - `src/games/eval_cont_ml1/syntax.rs` に pretty-printer の leaf/nested テストを追加した。
      - `src/games/eval_cont_ml1/prover.rs` に正常系/異常系/fixture 形状比較テストを追加した。
      - `src/lib.rs` に `prover --game EvalContML1` の正常系/異常系/round-trip テストを追加した。
    - ドキュメント:
      - `README.md` / `docs/design.md` / `AGENTS.md` / `docs/PLAN.md` を同期した。
    - R1:
      - Finding: `C-Ret` の空 continuation を tail 継承（`explicit_ret = false`）のまま生成すると fixture 形状比較で不一致になった。
      - Action: `src/games/eval_cont_ml1/prover.rs` で `C-Ret` 生成時の continuation を常に `hole()` に正規化した。
      - Scope: in-scope
      - Backlog: なし
    - R2:
      - Finding: 指摘なし
      - Action: なし
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
- [ ] `21` [P2][Implementation] `EvalContML4` prover を実装する（judgment-only parser / prover 本体 / pretty-printer / CLI 経路 / round-trip テスト）。
- [ ] `22` [P2][Implementation] `TypingML4` prover を実装する（judgment-only parser / prover 本体 / pretty-printer / CLI 経路 / round-trip テスト）。
- [ ] `23` [P2][Implementation] `PolyTypingML4` prover を実装する（judgment-only parser / prover 本体 / pretty-printer / CLI 経路 / round-trip テスト）。
- [ ] `24` [P2][Implementation] `NamelessML3` prover を実装する（judgment-only parser / prover 本体 / pretty-printer / CLI 経路 / round-trip テスト）。
- [ ] `25` [P2][Implementation] `EvalNamelessML3` prover を実装する（judgment-only parser / prover 本体 / pretty-printer / CLI 経路 / round-trip テスト）。
- [ ] `26` [P2][Implementation] `EvalNatExp` prover を実装する（judgment-only parser / prover 本体 / pretty-printer / CLI 経路 / round-trip テスト）。
- [ ] `27` [P2][Implementation] `ReduceNatExp` prover を実装する（judgment-only parser / prover 本体 / pretty-printer / CLI 経路 / round-trip テスト）。

#### 共通完了条件（Implementation タスク）

- 対象 game の `prover --game <name>` で CLI が受理される。
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
  - [2026-02-19 Prover 実装計画 完了タスクアーカイブ（01-17）](plans/2026-02-19-prover-roadmap-completed-01-17.md)
  - [2026-02-16 Checker 実装ロードマップ（完了）](plans/2026-02-16-checker-roadmap-completed.md)
  - [2026-02-15 Checker 実装ロードマップ](plans/2026-02-15-checker-roadmap.md)
  - [2026-02-14 Nat Checker 実装計画](plans/2026-02-14-nat-checker.md)
