# PLAN

最終更新日: 2026-02-20
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

### ML 系 `app`/`cons` 優先順位修正計画

最終更新日: 2026-02-20
この計画のスコープ: ML 系 game の式 `f 1::2::[]` を `(f 1)::2::[]` と解釈する仕様を parser / pretty-printer / prover round-trip まで一貫させる。

#### 背景

- 現行実装では ML 系 game の `app` と `cons` の優先順位が仕様どおりかを直接固定する回帰テストが不足している。
- 目標は `f 1::2::[]` を `f (1::2::[])` ではなく `(f 1)::2::[]` と扱うこと。

#### 対象スコープ

- 対象 game: `EvalML4` / `EvalML5` / `EvalContML4` / `TypingML4` / `PolyTypingML4`
- parser の `app`/`cons` 優先順位
- syntax の `Display`（括弧付与規則）
- `prover -> checker` round-trip 回帰

#### 非スコープ

- list 構文を持たない game への横展開
- evaluator/checker の規則追加
- CLI 契約変更

#### 実装方針

- t-wada スタイル TDD（`Red -> Green -> Refactor`）で進める。
- 先に parser/syntax/lib の回帰テストを追加し、失敗を確認してから最小実装で修正する。
- 変更は対象 5 game に限定し、共通化は今回必須の範囲を超えて導入しない（`YAGNI`）。

#### バックログ（着手優先順）

- 運用ルール:
  - 未完了タスクは上から順に着手する。
  - 優先度は `P1`（最優先）/ `P2`（中優先）/ `P3`（低優先）で表記する。
  - 順序を入れ替える場合は、この節に理由を追記する。
  - 実装完了後は `AGENTS.md` の Design Principles にある判断基準（高凝集・低結合 / `YAGNI` / `KISS`）で 5 回レビューし、各回の改善内容（または指摘なし）を完了メモに記録する。

- [x] `01` [P1][Test] parser 回帰テストを追加し、`f 1::2::[]` が `(f 1)::2::[]` にパースされることを固定する（対象 5 game）。
  - 完了メモ（2026-02-20）:
    - 実装:
      - `EvalML4` / `EvalML5` / `EvalContML4` / `TypingML4` / `PolyTypingML4` の `parser.rs` に `parses_application_tighter_than_cons` を追加。
    - テスト:
      - `cargo test parses_application_tighter_than_cons`
    - ドキュメント:
      - `docs/PLAN.md` の `01` を完了へ更新。
    - R1:
      - Finding: 指摘なし
      - Action: なし
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
- [x] `02` [P1][Implementation] parser を修正し、`app` が `cons` より強く結合するよう統一する（対象 5 game）。
  - 完了メモ（2026-02-20）:
    - 実装:
      - `EvalML4` / `EvalML5` / `EvalContML4` / `TypingML4` / `PolyTypingML4` の parser 実装を確認し、`parse_cons_expr` が `parse_lt_expr` を、`parse_mul_expr` が `parse_app_expr` を用いる構造で統一済みであることを確認。
      - 追加実装は不要（既存実装が要件を満たしていたためコード変更なし）。
    - テスト:
      - `cargo test parses_application_tighter_than_cons`
    - ドキュメント:
      - `docs/PLAN.md` の `02` を完了へ更新。
    - R1:
      - Finding: 指摘なし
      - Action: なし
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
- [x] `03` [P1][Test] syntax 回帰テストを追加し、`App(f, Cons(...))` と `Cons(App(f,1), ...)` の括弧出力を固定する（対象 5 game）。
  - 完了メモ（2026-02-20）:
    - 実装:
      - `EvalML4` / `EvalML5` / `EvalContML4` / `TypingML4` / `PolyTypingML4` の `syntax.rs` テストに `formats_app_and_cons_with_expected_parentheses` を追加。
      - `App(f, Cons(...))` の表示を `f (1 :: 2 :: [])`、`Cons(App(f,1), ...)` の表示を `f 1 :: 2 :: []` として固定。
    - テスト:
      - `cargo test formats_app_and_cons_with_expected_parentheses`
    - ドキュメント:
      - `docs/PLAN.md` の `03` を完了へ更新。
    - R1:
      - Finding: 指摘なし
      - Action: なし
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
- [x] `04` [P1][Implementation] syntax の precedence / `fmt_with_precedence` を修正し、`prover` 出力で優先順位が崩れないようにする（対象 5 game）。
  - 完了メモ（2026-02-20）:
    - 実装:
      - `EvalML4` / `EvalML5` / `EvalContML4` / `TypingML4` / `PolyTypingML4` の `syntax.rs` 実装を確認し、`Expr::precedence` が `App=5` / `Cons=1` で統一され、`fmt_with_precedence` が `App` 引数側で `self.precedence() + 1` を用いて括弧付与していることを確認。
      - 追加実装は不要（既存実装が要件を満たしていたためコード変更なし）。
      - 実地確認として、上記 5 game で `App(f, Cons(...))` / `Cons(App(f,1), ...)` を含む judgment を `prover` で導出し、その出力を `checker` へ再入力して全ケースで受理されることを確認。
    - テスト:
      - `cargo test formats_app_and_cons_with_expected_parentheses`
    - ドキュメント:
      - `docs/PLAN.md` の `04` を完了へ更新。
    - R1:
      - Finding: 指摘なし
      - Action: なし
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
- [x] `05` [P1][Test] `src/lib.rs` に `prover -> checker` round-trip 回帰を追加し、`app`/`cons` 混在入力で AST 変形が起きないことを固定する（対象 5 game）。
  - 完了メモ（2026-02-20）:
    - 実装:
      - `src/lib.rs` の test module に `assert_prover_output_round_trips_to_checker_root_judgment` を追加し、`prover -> checker` round-trip の重複ロジックを共通化。
      - 対象 5 game（`EvalML4` / `EvalML5` / `EvalContML4` / `TypingML4` / `PolyTypingML4`）に `app`/`cons` 混在入力の round-trip 回帰テスト（`*_app_cons_mixed_expression`）を追加。
      - テスト時のみの入力上限（`MAX_INPUT_BYTES`）を `16 * 1024` に引き上げ、`EvalContML4` の round-trip 回帰テストが導出木サイズ上限で失敗しないように調整（実行時上限 `8 * 1024 * 1024` は変更なし）。
    - テスト:
      - `cargo test app_cons_mixed_expression`
      - `cargo test rejects_oversized_input`
    - ドキュメント:
      - `docs/PLAN.md` の `05` を完了へ更新。
    - R1:
      - Finding: 指摘なし
      - Action: なし
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
- [x] `06` [P1][Validation] `cargo fmt` / `cargo test` / `cargo clippy --all-targets --all-features -- -D warnings` を通し、完了メモ（R1-R5 含む）を更新する。
  - 完了メモ（2026-02-20）:
    - 実装:
      - 追加のコード修正はなし（`05` での変更を対象に検証を実施）。
    - テスト:
      - `cargo test`
    - ドキュメント:
      - `docs/PLAN.md` の `06` を完了へ更新。
    - R1:
      - Finding: 指摘なし
      - Action: なし
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

### `games/{GAME}.html` 差分反映計画（未対応 game 追加）

最終更新日: 2026-02-20
この計画のスコープ: 公開されている `https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/{GAME}.html` の syntax を基準に、現行実装の対象外 game をバックログへ追加する。

#### 背景

- 現行実装（`checker`/`prover`）は 18 game 対応だが、公開 syntax ページには未対応 game が追加で存在する。
- 実装対象の見落としを防ぐため、確認済み差分を `docs/PLAN.md` の着手順バックログに明示する。

#### 差分確認メモ（2026-02-20）

- 公開ページ実在を確認した未対応 game:
  - `EvalML6`
  - `TypingML2`
  - `TypingML3`
  - `TypingML5`
  - `TypingML6`
  - `PolyTypingML3`
  - `EvalRefML3`
- `TypingML5` は `<title>` / `<h1>` が欠落しているが、syntax と rule 本体は公開されていることを確認。

#### バックログ（着手優先順）

- 運用ルール:
  - 未完了タスクは上から順に着手する。
  - 優先度は `P1`（最優先）/ `P2`（中優先）/ `P3`（低優先）で表記する。
  - 順序を入れ替える場合は、この節に理由を追記する。

- [ ] `01` [P1][Docs] 未対応 game 7 件の実装方針（checker/prover 対応順・依存関係）を確定し、`docs/design.md` と整合させる。
- [ ] `02` [P1][Implementation] `EvalML6` を `checker`/`prover` の対象に追加し、最小導出ケースの回帰テストを追加する。
- [ ] `03` [P1][Implementation] `TypingML2` / `TypingML3` を `checker`/`prover` の対象に追加し、型付け規則ごとの最小回帰テストを追加する。
- [ ] `04` [P1][Implementation] `TypingML5` / `TypingML6` を `checker`/`prover` の対象に追加し、list/拡張構文を含む回帰テストを追加する。
- [ ] `05` [P1][Implementation] `PolyTypingML3` を `checker`/`prover` の対象に追加し、多相型環境の最小回帰テストを追加する。
- [ ] `06` [P1][Implementation] `EvalRefML3` を `checker`/`prover` の対象に追加し、store を伴う評価規則の回帰テストを追加する。
- [ ] `07` [P1][Validation] 追加 game 対応後に `cargo fmt` / `cargo test` / `cargo clippy --all-targets --all-features -- -D warnings` を通し、完了メモ（R1-R5）を更新する。

## 履歴計画

- 完了または凍結した計画は `docs/plans/` に保存する。
- このファイルには現在の計画のみを保持する。
- 履歴一覧:
  - [2026-02-20 Prover 実装計画（Nat-first）完了アーカイブ](plans/2026-02-20-prover-roadmap-completed.md)
  - [2026-02-19 Prover 実装計画 完了タスクアーカイブ（01-17）](plans/2026-02-19-prover-roadmap-completed-01-17.md)
  - [2026-02-16 Checker 実装ロードマップ（完了）](plans/2026-02-16-checker-roadmap-completed.md)
  - [2026-02-15 Checker 実装ロードマップ](plans/2026-02-15-checker-roadmap.md)
  - [2026-02-14 Nat Checker 実装計画](plans/2026-02-14-nat-checker.md)
