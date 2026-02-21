# PLAN

最終更新日: 2026-02-21
このファイルは現在計画の単一ソースです。

## 運用ルール

- 現在の計画・進捗・改善バックログはこのファイルだけを更新する。
- 実装スコープ（対象機能・着手順・pending 判断）の正本はこのファイルとし、`AGENTS.md` には重複記載しない。
- 変更時は関連ドキュメント（`README.md` / `docs/design.md` / `AGENTS.md` / ADR）と同一変更セットで同期する。
- タスク実行中に当該タスクのスコープ外の改善を発見した場合は、バックログに追加し、優先度と依存関係を考慮して適切な位置へ挿入する。
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

### EvalContML4 負数引数の互換修正計画

最終更新日: 2026-02-21
この計画のスコープ: `EvalContML4` の `prover` 出力を copl-tools 互換に修正し、`checker` 側でも同種の非互換フォーマットを検出できるようにする。

#### 背景

- `cat copl/137.exam.copl | copl-rs prover --game EvalContML4` の出力を本家 `copl-tools` checker に入力すると、`E-App` / `C-EvalArg` の conclusion form error が発生する。
- 同じ出力を `copl-rs checker --game EvalContML4` に入力すると valid になってしまい、互換性のない形を見逃している。
- 原因は関数適用の負数引数が `f -2` / `{... _ -2}` 形式で出力・受理される点にある。

#### 対象スコープ

- `src/games/eval_cont_ml4/syntax.rs`
- `src/games/eval_cont_ml4/parser.rs`
- `src/games/eval_cont_ml4/prover.rs`
- `src/lib.rs`
- `docs/PLAN.md` と `docs/plans/` の同期

#### 非スコープ

- `EvalContML4` 以外の game への同時展開
- CLI 契約変更
- 規則追加や証明探索戦略の変更

#### 実装方針

- t-wada スタイル TDD（`Red -> Green -> Refactor`）で進める。
- 先に回帰テストを追加し、`f -2` / `{... _ -2}` を失敗として固定してから最小実装で修正する。
- 変更は `EvalContML4` に限定し、他 game への抽象化は導入しない（`YAGNI`）。

#### バックログ（着手優先順）

- 運用ルール:
  - 未完了タスクは上から順に着手する。
  - 優先度は `P1`（最優先）/ `P2`（中優先）/ `P3`（低優先）で表記する。
  - 順序を入れ替える場合は、この節に理由を追記する。
  - 実装完了後は `AGENTS.md` の Design Principles にある判断基準（高凝集・低結合 / `YAGNI` / `KISS`）で 5 回レビューし、各回の改善内容（または指摘なし）を完了メモに記録する。

- [x] `01` [P1][Test] `EvalContML4` parser/syntax/lib/prover に「負数引数は括弧必須」の回帰テストを追加する。
  - 完了メモ（2026-02-21）:
    - 実装:
      - `src/games/eval_cont_ml4/parser.rs` に `f -2` と `{... _ -2}` を拒否する回帰テストを追加。
      - `src/games/eval_cont_ml4/syntax.rs` に `f (-2)` および `{... _ (-2)}` 出力を固定する回帰テストを追加。
      - `src/games/eval_cont_ml4/prover.rs` に `copl/137.exam.copl` 由来の出力形回帰テストを追加。
      - `src/lib.rs` に `checker --game EvalContML4` で `f -2` を拒否する回帰テストを追加。
    - テスト:
      - `cargo test`
    - ドキュメント:
      - `docs/PLAN.md` の `01` を完了へ更新。
- [x] `02` [P1][Implementation] `syntax` の出力を修正し、関数適用引数の負整数を `(-n)` 形式で出力する。
  - 完了メモ（2026-02-21）:
    - 実装:
      - `src/games/eval_cont_ml4/syntax.rs` の `fmt_with_precedence` を修正し、適用引数位置の負整数に括弧を付けるように変更。
    - テスト:
      - `cargo test`
    - ドキュメント:
      - `docs/PLAN.md` の `02` を完了へ更新。
- [x] `03` [P1][Implementation] `parser` を修正し、`f -2` / `{... _ -2}` を parse error として拒否する。
  - 完了メモ（2026-02-21）:
    - 実装:
      - `src/games/eval_cont_ml4/parser.rs` に負整数の非括弧適用引数を拒否するガード（`reject_unparenthesized_negative_app_argument`）を追加。
    - テスト:
      - `cargo test`
    - ドキュメント:
      - `docs/PLAN.md` の `03` を完了へ更新。
- [x] `04` [P1][Validation] `cargo fmt` / `cargo test` / `cargo clippy --all-targets --all-features -- -D warnings` を通し、完了メモ（R1-R5）を更新する。
  - 完了メモ（2026-02-21）:
    - 実装:
      - `EvalContML4` の `prover` 出力を `f (-2)` / `{... _ (-2)}` 形式へ修正。
      - `EvalContML4` `checker` が `f -2` / `{... _ -2}` を parse error として拒否するよう修正。
      - 現在の `PLAN` を `docs/plans/2026-02-21-eval-cont-ml4-negative-app-arg-fix-pre-archive.md` にアーカイブし、修正計画を `docs/PLAN.md` に再作成。
    - テスト:
      - `cargo fmt`
      - `cargo test`
      - `cargo clippy --all-targets --all-features -- -D warnings`
    - ドキュメント:
      - `docs/PLAN.md`
      - `docs/plans/2026-02-21-eval-cont-ml4-negative-app-arg-fix-pre-archive.md`
    - R1:
      - Finding: `EvalContML4` の適用引数における負整数の括弧有無が `copl-tools` 互換性に影響するが、回帰テストで固定されていなかった。
      - Action: parser/syntax/prover/lib に負数引数専用の回帰テストを追加。
      - Scope: in-scope
      - Backlog: なし
    - R2:
      - Finding: 出力側のみ修正すると checker が旧フォーマットを受理し続け、互換違反を見逃す。
      - Action: parser で `f -2` / `{... _ -2}` を拒否し checker 経路でも検知するようにした。
      - Scope: in-scope
      - Backlog: なし
    - R3:
      - Finding: 修正範囲を `EvalContML4` に限定したため、同系 game に同種ギャップが残る可能性。
      - Action: out-of-scope と判断し、`05` として改善バックログを追加。
      - Scope: out-of-scope
      - Backlog: 05
    - R4:
      - Finding: `PLAN` の現行計画と履歴計画の分離が必要。
      - Action: 旧 `PLAN` を `docs/plans/` にアーカイブし、現行計画を最小差分で再構成。
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
- [x] `05` [P2][Improvement] `EvalML3` / `EvalML4` / `EvalML5` / `TypingML4` / `PolyTypingML4` でも負数の適用引数出力・受理仕様を点検し、`copl-tools` 互換性の有無を確認する。
  - 理由: `EvalContML4` と同じ `App` 表示ロジックを使う game 群に同種の互換性ギャップが残っている可能性があるため。
  - 完了メモ（2026-02-21）:
    - 実装:
      - `src/games/eval_ml3/parser.rs` / `src/games/eval_ml4/parser.rs` / `src/games/eval_ml5/parser.rs` / `src/games/typing_ml4/parser.rs` / `src/games/poly_typing_ml4/parser.rs` に、非括弧の負数適用引数を拒否するガードを追加。
      - `src/games/eval_ml3/syntax.rs` / `src/games/eval_ml4/syntax.rs` / `src/games/eval_ml5/syntax.rs` / `src/games/typing_ml4/syntax.rs` / `src/games/poly_typing_ml4/syntax.rs` に、負数適用引数を `(-n)` 形式で出力する修正を追加。
      - `src/lib.rs` に 5 game の `checker` 経路で `f -2` を拒否する回帰テストを追加。
    - テスト:
      - `cargo test -q negative_int_argument`
      - `cargo test`
    - ドキュメント:
      - `docs/PLAN.md` の `05` を完了へ更新。
    - R1:
      - Finding: 5 game で `prover` が `f -2` を出力し、`checker` も同形式を受理していた。
      - Action: parser と syntax を `EvalContML4` と同等方針で修正し、回帰テストを追加。
      - Scope: in-scope
      - Backlog: なし
    - R2:
      - Finding: Typing 系 game では `Type` formatter と `Expr` formatter が同名で、誤って `Type` 側を触るリスクがある。
      - Action: `Expr` 側の `fmt_with_precedence` のみに修正を限定し、テストで検証。
      - Scope: in-scope
      - Backlog: なし
    - R3:
      - Finding: `lib.rs` に game 横断の checker 回帰が不足し、parser 修正の取りこぼし検知が弱い。
      - Action: `assert_checker_rejects_unparenthesized_negative_app_argument` を追加し 5 game を一括検証。
      - Scope: in-scope
      - Backlog: なし
    - R4:
      - Finding: `App` 表示ロジックを持つ Nameless 系（`NamelessML3` / `EvalNamelessML3`）にも同種の互換性ギャップが残る可能性。
      - Action: out-of-scope として次タスク化。
      - Scope: out-of-scope
      - Backlog: 06
    - R5:
      - Finding: 指摘なし
      - Action: なし
      - Scope: in-scope
      - Backlog: なし
    - 検証:
      - `cargo fmt`: pass
      - `cargo test`: pass
      - `cargo clippy --all-targets --all-features -- -D warnings`: pass
- [x] `06` [P2][Improvement] `NamelessML3` / `EvalNamelessML3` の `App` でも負数適用引数の出力・受理仕様を点検し、必要なら `(-n)` へ統一する。
  - 理由: 同じ `App` precedence/formatter 構造を持つため、今回と同型の互換性不一致が残っている可能性がある。
  - 完了メモ（2026-02-21）:
    - 実装:
      - `src/games/nameless_ml3/parser.rs` / `src/games/eval_nameless_ml3/parser.rs` に、非括弧の負数適用引数を拒否するガードを追加。
      - `src/games/nameless_ml3/syntax.rs` / `src/games/eval_nameless_ml3/syntax.rs` に、負数適用引数を `(-n)` 形式で出力する修正を追加。
      - `src/lib.rs` に `NamelessML3` / `EvalNamelessML3` の checker 回帰テストを追加。
    - テスト:
      - `cargo test -q negative_int_argument`
      - `cargo test`
    - ドキュメント:
      - `docs/PLAN.md` の `06` を完了へ更新。
    - R1:
      - Finding: `NamelessML3` / `EvalNamelessML3` でも `prover` が `f -2` / `#1 -2` を出力しうる。
      - Action: syntax 側で負数適用引数の括弧付与を追加し、出力を `(-n)` に統一。
      - Scope: in-scope
      - Backlog: なし
    - R2:
      - Finding: parser が旧形式を受理すると checker 互換性検知ができない。
      - Action: parser に共通ガードを追加し、`negative integer application arguments must be parenthesized` で拒否。
      - Scope: in-scope
      - Backlog: なし
    - R3:
      - Finding: `NamelessML3` には `NamedExpr` と `NamelessExpr` の 2 系統 `App` formatter がある。
      - Action: 両方に同じ括弧付与ルールを実装し、専用テストを追加。
      - Scope: in-scope
      - Backlog: なし
    - R4:
      - Finding: `lib.rs` の checker 回帰が game 個別追加だと重複しやすい。
      - Action: 既存 helper (`assert_checker_rejects_unparenthesized_negative_app_argument`) を流用して最小差分で追加。
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

## 履歴計画

- 完了または凍結した計画は `docs/plans/` に保存する。
- このファイルには現在の計画のみを保持する。
- 履歴一覧:
  - [2026-02-21 EvalContML4 修正前 PLAN アーカイブ](plans/2026-02-21-eval-cont-ml4-negative-app-arg-fix-pre-archive.md)
  - [2026-02-20 Prover 実装計画（Nat-first）完了アーカイブ](plans/2026-02-20-prover-roadmap-completed.md)
  - [2026-02-19 Prover 実装計画 完了タスクアーカイブ（01-17）](plans/2026-02-19-prover-roadmap-completed-01-17.md)
  - [2026-02-16 Checker 実装ロードマップ（完了）](plans/2026-02-16-checker-roadmap-completed.md)
  - [2026-02-15 Checker 実装ロードマップ](plans/2026-02-15-checker-roadmap.md)
  - [2026-02-14 Nat Checker 実装計画](plans/2026-02-14-nat-checker.md)
