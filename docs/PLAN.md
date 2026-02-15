# PLAN

最終更新日: 2026-02-16
このファイルは現在計画の単一ソースです。

## 運用ルール

- 現在の計画・進捗・改善バックログはこのファイルだけを更新する。
- 実装スコープ（対象 game・着手順・pending 判断）の正本はこのファイルとし、`AGENTS.md` には重複記載しない。
- 変更時は関連ドキュメント（README / design / AGENTS など）と同一変更セットで同期する。

## 現在の計画

### Checker 実装ロードマップ（複数 game）

最終更新日: 2026-02-16
このフェーズのスコープ: 未実装 checker 5 game の段階的実装計画

#### 背景

ユーザー要望の対象 game 一覧（18件）:
- CompareNat1
- CompareNat2
- CompareNat3
- EvalContML1
- EvalContML4
- EvalML1
- EvalML1Err
- EvalML2
- EvalML3
- EvalML4
- EvalML5
- EvalNamelessML3
- EvalNatExp
- NamelessML3
- Nat
- PolyTypingML4
- ReduceNatExp
- TypingML4

現状の実装済み（計画対象から除外）:
- [x] Nat
- [x] CompareNat1
- [x] CompareNat2
- [x] CompareNat3
- [x] EvalML1
- [x] EvalML1Err
- [x] EvalML2
- [x] EvalML3
- [x] EvalML4
- [x] NamelessML3
- [x] EvalNamelessML3
- [x] EvalNatExp
- [x] ReduceNatExp

今後の実装対象（計5件）:
- [ ] EvalML5
- [ ] EvalContML1
- [ ] EvalContML4
- [ ] TypingML4
- [ ] PolyTypingML4

#### 参照仕様

- 各 game の規則定義は以下:
  - `https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/{game}.html`
- 2026-02-15 時点で上記 18 game のページ到達性（HTTP 200）を確認済み。

#### 実装方針（共通）

- 既存方針を継続する。
  - 単一 crate、`src/main.rs` は薄く、ロジックは `src/lib.rs`。
  - game ごとに `src/games/<game>/syntax.rs | lexer.rs | parser.rs | checker.rs`。
  - parser は手書き再帰下降、raw rule name を保持し、rule 解決は checker で行う。
  - 成功時は root judgment のプレーンテキストを返す。
- TDD サイクルを game 単位で小さく回す。
  - Red: 最小失敗テスト（まず fixture 1件）
  - Green: 最小実装で通す
  - Refactor: 規則検証の重複整理
- 実装着手順は `copl/` のファイル番号が若いものを優先する（昇順）。
- 1 game 完了の定義:
  - `--game <name>` で CLI 受理される。
  - 対応 fixture が全通。
  - 代表的な異常系（unknown rule / arity mismatch / rule mismatch）が通る。

#### 統合バックログ（着手優先順）

- 運用ルール:
  - 未完了タスクは上から順に着手する。
  - 優先度は `P1`（最優先）/ `P2`（中優先）/ `P3`（低優先）で表記する。
  - 順序を入れ替える場合は、この節に理由を追記する。

- [x] `01` [P1][Implementation] `EvalNatExp` checker を実装する（`copl/015.copl` - `copl/020.copl`）。
- [x] `02` [P1][Implementation] `ReduceNatExp` checker を実装する（`copl/021.copl` - `copl/024.copl`）。
- [x] `03` [P1][Improvement] `RuleViolation` 診断で failing premise path（どの前提で失敗したか）を明示する。
- [x] `04` [P2][Implementation] `EvalML1` checker を実装する（`copl/025.copl` - `copl/030.copl`）。
- [x] `05` [P2][Implementation] `EvalML1Err` checker を実装する（`copl/031.copl` - `copl/033.copl`）。
- [x] `06` [P2][Implementation] `EvalML2` checker を実装する（`copl/034.copl` - `copl/039.copl`）。
- [x] `07` [P2][Improvement] 異常系 fixture（unknown rule / arity mismatch / rule mismatch）を実装済み game 全体で拡張する。  
  完了メモ（2026-02-16）: 実装済み game（Nat / CompareNat1 / CompareNat2 / CompareNat3 / EvalML1 / EvalML1Err / EvalML2 / EvalNatExp / ReduceNatExp）の checker テストに、上記3カテゴリを網羅する異常系ケースが揃っていることを確認。
- [x] `07a` [P2][Improvement] Nat 系算術規則（`P-*` / `T-*`）の checker 検証ロジックを共通化する（理由: `Nat` / `EvalNatExp` / `ReduceNatExp` で同等ロジックが重複したため）。  
  完了メモ（2026-02-16）: `src/games/nat_arith.rs` を追加し、`Nat` / `EvalNatExp` / `ReduceNatExp` の `P-Zero` / `P-Succ` / `T-Zero` / `T-Succ` の構造チェックを共通ヘルパー化。既存の checker テスト・fixture テスト・clippy を通過。
- [x] `07b` [P2][Improvement] `ReduceNatExp` の `R-*` / `DR-*` checker 検証ロジックを共通化する（理由: 1-step 関係と deterministic 1-step 関係で同型の検証分岐が重複したため）。  
  完了メモ（2026-02-16）: `ReduceNatExp` checker 内で 1-step 関係（`R-*`）と deterministic 1-step 関係（`DR-*`）の同型検証を共通ヘルパー化し、演算種別（`+` / `*`）と関係種別（`--->` / `-d->`）を切り替える構造へ整理。既存の checker テスト・fixture テスト・clippy を通過。
- [x] `08` [P2][Implementation] `EvalML3` checker を実装する（`copl/040.copl` - `copl/053.copl`）。  
  完了メモ（2026-02-16）: `EvalML3` 用モジュール（`syntax` / `lexer` / `parser` / `checker`）を追加し、`E-LetRec` / `E-Fun` / `E-App` / `E-AppRec` を含む `E-*` / `B-*` の規則検証を実装。`copl/040.copl` - `copl/053.copl` の fixture 通過と、unknown rule / arity mismatch / rule mismatch の異常系テストを追加して検証済み。
- [x] `09` [P2][Implementation] `NamelessML3` checker を実装する（`copl/054.copl`, `056.copl`, `058.copl`, `060.copl`, `062.copl`, `064.copl`, `066.copl`, `068.copl`）。  
  完了メモ（2026-02-16）: `NamelessML3` 用モジュール（`syntax` / `lexer` / `parser` / `checker`）を追加し、`Tr-Int` / `Tr-Bool` / `Tr-Var1` / `Tr-Var2` / `Tr-If` / `Tr-Plus` / `Tr-Minus` / `Tr-Times` / `Tr-Lt` / `Tr-Let` / `Tr-Fun` / `Tr-App` / `Tr-LetRec` の規則検証を実装。`copl/054.copl`, `056.copl`, `058.copl`, `060.copl`, `062.copl`, `064.copl`, `066.copl`, `068.copl` の fixture 通過と、unknown rule / arity mismatch / rule mismatch の異常系テストを追加して検証済み。
- [x] `10` [P2][Implementation] `EvalNamelessML3` checker を実装する（`copl/055.copl`, `057.copl`, `059.copl`, `061.copl`, `063.copl`, `065.copl`, `067.copl`, `069.copl`）。  
  完了メモ（2026-02-16）: `EvalNamelessML3` 用モジュール（`syntax` / `lexer` / `parser` / `checker`）を追加し、de Bruijn index（`#n`）と nameless closure（`fun . -> ...` / `rec . = fun . -> ...`）を扱う `E-*` / `B-*` 規則検証（`E-Var`, `E-AppRec`, `E-LetRec` を含む）を実装。`copl/055.copl`, `057.copl`, `059.copl`, `061.copl`, `063.copl`, `065.copl`, `067.copl`, `069.copl` の fixture 通過と、unknown rule / arity mismatch / rule mismatch の異常系テストを追加して検証済み。
- [x] `11` [P2][Implementation] `EvalML4` checker を実装する（`copl/070.copl` - `copl/077.copl`）。  
  完了メモ（2026-02-16）: `EvalML4` 用モジュール（`syntax` / `lexer` / `parser` / `checker`）を追加し、リスト値（`[]`, `::`）とパターンマッチ（`match ... with [] -> ... | x :: y -> ...`）を含む `E-*` / `B-*` 規則検証（`E-Var`, `E-Nil`, `E-Cons`, `E-MatchNil`, `E-MatchCons` を含む）を実装。`copl/070.copl` - `copl/077.copl` の fixture 通過と、unknown rule / arity mismatch / rule mismatch の異常系テストを追加して検証済み。
- [ ] `12` [P2][Implementation] `EvalML5` checker を実装する（`copl/078.copl`, `copl/079.copl`）。
- [ ] `13` [P2][Implementation] `TypingML4` checker を実装する（`copl/080.copl` - `copl/106.copl`）。
- [ ] `14` [P2][Implementation] `PolyTypingML4` checker を実装する（`copl/107.copl` - `copl/123.copl`）。
- [ ] `15` [P2][Implementation] `EvalContML1` checker を実装する（`copl/124.copl` - `copl/129.copl`）。
- [ ] `16` [P2][Implementation] `EvalContML4` checker を実装する（`copl/130.copl` - `copl/140.copl`）。
- [ ] `17` [P3][Improvement] `resolver` 着手前に CLI パーサのサブコマンド分割方針を確定し、必要なら分離リファクタを先行する。

#### 共通完了条件（Implementation タスク）

- `--game <name>` で CLI が受理する。
- 対応 fixture が全通する。
- 代表的な異常系（unknown rule / arity mismatch / rule mismatch）が通る。
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
  - [2026-02-15 Checker 実装ロードマップ](plans/2026-02-15-checker-roadmap.md)
  - [2026-02-14 Nat Checker 実装計画](plans/2026-02-14-nat-checker.md)
