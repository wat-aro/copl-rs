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

### EvalRefML3 checker/prover 完全対応計画

最終更新日: 2026-02-21
この計画のスコープ: `EvalRefML3` の `checker` / `prover` を本家仕様（`EvalRefML3.html`）および `copl/141.copl` から `copl/145.copl` に整合する形へ拡張し、過去に発生した互換性不具合を再発させない。

#### 背景

- 現状の `EvalRefML3` は `E-Int` / `E-Unit` / `E-Loc` / `E-Var` / `E-Let` / `E-Ref` / `E-Deref` / `E-Assign` の最小実装に留まっており、`E-If*` / `E-Plus` / `E-App` / `E-LetRec` / `E-AppRec` / `B-*` を含む導出を扱えない。
- `copl/141.copl` から `copl/145.copl` は、参照・代入・関数・クロージャ・条件分岐・算術・比較を横断するため、現状仕様ギャップを埋める回帰セットとして適している。
- 過去に `EvalContML4` などで「prover 出力は通るが本家 checker 互換で崩れる」問題を修正しており、`EvalRefML3` でも同じ種類のズレを先に防ぐ必要がある。

#### 対象スコープ

- `src/games/eval_ref_ml3/lexer.rs`
- `src/games/eval_ref_ml3/parser.rs`
- `src/games/eval_ref_ml3/syntax.rs`
- `src/games/eval_ref_ml3/checker.rs`
- `src/games/eval_ref_ml3/prover.rs`
- `src/lib.rs`
- `docs/PLAN.md`（進捗・完了メモ更新）
- 必要時: `README.md` / `docs/design.md` / ADR

#### 非スコープ

- `EvalRefML3` 以外の game への同時展開
- CLI 契約変更
- JSON 出力モード追加

#### 実装方針

- t-wada スタイル TDD（`Red -> Green -> Refactor`）で進める。
- 先に `copl/141-145` と judgment-only 入力の失敗テストを追加し、最小実装で順次通す。
- `checker` と `prover` の整合を常に維持し、`prover` 出力を `checker` に戻して root judgment 一致を確認する。
- 過去不具合の再発防止を設計要件に含める:
  - 関数適用の負数引数は `(-n)` 表示に統一し、非括弧形（`f -2`）を parser で拒否する。
  - 優先順位・結合規則（特に `App` / 単項 `!` / `ref` / 代入 `:=` / 二項演算）を pretty-printer と parser で一致させる。
  - checker が受理する形と prover が出力する形の乖離を、統合テストで常時検知する。
  - CoPL ページ記載と実 fixture の差異（表記揺れ/誤植の可能性）は `copl/141-145` と本家 checker 挙動を優先して判断する。

#### バックログ（着手優先順）

- 運用ルール:
  - 未完了タスクは上から順に着手する。
  - 優先度は `P1`（最優先）/ `P2`（中優先）/ `P3`（低優先）で表記する。
  - 順序を入れ替える場合は、この節に理由を追記する。
  - 実装完了後は `AGENTS.md` の Design Principles にある判断基準（高凝集・低結合 / `YAGNI` / `KISS`）で 5 回レビューし、各回の改善内容（または指摘なし）を完了メモに記録する。

- [ ] `01` [P1][Test] `EvalRefML3` の現行不足を固定する失敗テストを追加する（checker/prover 共通）。
  - 内容:
    - `copl/141-145` を checker へ通す回帰テスト。
    - judgment-only で `if` / `fun` / `app` / `let rec` / `B-*` を含む入力テスト。
    - `prover` 出力の checker round-trip テスト（store 効果込み）。
- [ ] `02` [P1][Checker-Parser] lexer/parser/syntax を仕様レベルへ拡張する（式・値・判定）。
  - 内容:
    - `bool`、`if then else`、`fun`、`app`、`let rec`、`+/-/*/<`、`B-* judgment` を構文追加。
    - closure 値 / rec closure 値 / store・env の既存形式との整合を定義。
    - parser/checker 境界ポリシーを維持し、未知 rule 名は checker で `RuleViolation` 化する。
- [ ] `03` [P1][Checker] `E-*` / `B-*` 規則検証を実装し、導出形チェックを強化する。
  - 内容:
    - 追加規則: `E-Bool`, `E-IfT`, `E-IfF`, `E-Plus`, `E-Minus`, `E-Times`, `E-Lt`, `E-Fun`, `E-App`, `E-LetRec`, `E-AppRec`, `B-Plus`, `B-Minus`, `B-Times`, `B-Lt`。
    - `E-Assign` の結論値を参照仕様どおりに扱う（`()` 固定にしない）。
    - `premise path` / `line:column` 付き診断を既存方針のまま維持する。
- [ ] `04` [P1][Prover] store-threading evaluator を規則追加分まで拡張する。
  - 内容:
    - closure/rec closure と `App`/`AppRec` の環境拡張を実装。
    - `if`・算術・比較・代入の逐次評価順序を `checker` と一致させる。
    - `E-Assign` の結果値・更新後 store を fixture と一致させる。
- [ ] `05` [P1][Format-Compat] 出力フォーマット互換の回帰を追加し、過去不具合を予防する。
  - 内容:
    - 負数適用引数の `(-n)` 出力/受理制約テストを `EvalRefML3` に追加。
    - 優先順位テスト（右結合代入、`!` と `app`、二項演算の括弧）を追加。
    - `prover` 出力を本家 checker に流して conclusion form error が出ないことを確認するテスト手順を整備。
- [ ] `06` [P1][Integration] `src/lib.rs` の route レベル回帰テストを拡充する。
  - 内容:
    - `checker --game EvalRefML3` の fixture 141-145 受理テスト。
    - `prover --game EvalRefML3` の non-derivable 診断テスト更新（expected/actual/fix）。
    - `prover` 出力の checker root judgment 一致テストを複数追加。
- [ ] `07` [P2][External-Validation] 本家 `copl-tools` との互換検証を実施し、差分を記録する。
  - 内容:
    - `copl-rs prover --game EvalRefML3` 出力を本家 checker に通して検証。
    - 差分が出た場合は `docs/PLAN.md` に原因・対処を追記し、必要なら追加タスク化。
- [ ] `08` [P1][Validation/Docs] 検証・文書同期・完了メモ（R1-R5）を更新する。
  - 内容:
    - `cargo fmt`
    - `cargo test`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `README.md` / `docs/design.md` / ADR（必要時）/ `docs/PLAN.md` の同期。

## 履歴計画

- 完了または凍結した計画は `docs/plans/` に保存する。
- このファイルには現在の計画のみを保持する。
- 履歴一覧:
  - [2026-02-21 EvalContML4 負数引数互換修正計画（完了）](plans/2026-02-21-eval-cont-ml4-negative-app-arg-fix-completed.md)
  - [2026-02-21 EvalContML4 修正前 PLAN アーカイブ](plans/2026-02-21-eval-cont-ml4-negative-app-arg-fix-pre-archive.md)
  - [2026-02-20 Prover 実装計画（Nat-first）完了アーカイブ](plans/2026-02-20-prover-roadmap-completed.md)
  - [2026-02-19 Prover 実装計画 完了タスクアーカイブ（01-17）](plans/2026-02-19-prover-roadmap-completed-01-17.md)
  - [2026-02-16 Checker 実装ロードマップ（完了）](plans/2026-02-16-checker-roadmap-completed.md)
  - [2026-02-15 Checker 実装ロードマップ](plans/2026-02-15-checker-roadmap.md)
  - [2026-02-14 Nat Checker 実装計画](plans/2026-02-14-nat-checker.md)
