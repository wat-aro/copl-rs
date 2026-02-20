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
- [ ] `02` [P1][Implementation] parser を修正し、`app` が `cons` より強く結合するよう統一する（対象 5 game）。
- [ ] `03` [P1][Test] syntax 回帰テストを追加し、`App(f, Cons(...))` と `Cons(App(f,1), ...)` の括弧出力を固定する（対象 5 game）。
- [ ] `04` [P1][Implementation] syntax の precedence / `fmt_with_precedence` を修正し、`prover` 出力で優先順位が崩れないようにする（対象 5 game）。
- [ ] `05` [P1][Test] `src/lib.rs` に `prover -> checker` round-trip 回帰を追加し、`app`/`cons` 混在入力で AST 変形が起きないことを固定する（対象 5 game）。
- [ ] `06` [P1][Validation] `cargo fmt` / `cargo test` / `cargo clippy --all-targets --all-features -- -D warnings` を通し、完了メモ（R1-R5 含む）を更新する。

## 履歴計画

- 完了または凍結した計画は `docs/plans/` に保存する。
- このファイルには現在の計画のみを保持する。
- 履歴一覧:
  - [2026-02-20 Prover 実装計画（Nat-first）完了アーカイブ](plans/2026-02-20-prover-roadmap-completed.md)
  - [2026-02-19 Prover 実装計画 完了タスクアーカイブ（01-17）](plans/2026-02-19-prover-roadmap-completed-01-17.md)
  - [2026-02-16 Checker 実装ロードマップ（完了）](plans/2026-02-16-checker-roadmap-completed.md)
  - [2026-02-15 Checker 実装ロードマップ](plans/2026-02-15-checker-roadmap.md)
  - [2026-02-14 Nat Checker 実装計画](plans/2026-02-14-nat-checker.md)
