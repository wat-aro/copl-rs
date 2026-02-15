# Checker 実装ロードマップ（複数 game）

最終更新日: 2026-02-15
このフェーズのスコープ: 未実装 checker 14 game の段階的実装計画

## 背景

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

今後の実装対象（計14件）:
- [ ] EvalNatExp
- [ ] ReduceNatExp
- [ ] EvalML1
- [ ] EvalML1Err
- [ ] EvalML2
- [ ] EvalML3
- [ ] NamelessML3
- [ ] EvalNamelessML3
- [ ] EvalML4
- [ ] EvalML5
- [ ] EvalContML1
- [ ] EvalContML4
- [ ] TypingML4
- [ ] PolyTypingML4

## 参照仕様

- 各 game の規則定義は以下:
  - `https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/{game}.html`
- 2026-02-15 時点で上記 18 game のページ到達性（HTTP 200）を確認済み。

## 実装方針（共通）

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

## マイルストン

### M1: NatExp 系

目的: 算術評価系を先に揃え、後続 ML 系実装の足場を安定化する。

対象:
- [ ] EvalNatExp（`copl/015.copl` - `copl/020.copl`）
- [ ] ReduceNatExp（`copl/021.copl` - `copl/024.copl`）

完了条件:
- 2 game とも checker が実装され、fixture が全通する。

### M2: ML 評価の基本層

目的: ルール数が比較的少ない層から ML 評価系列を立ち上げる。

対象:
- [ ] EvalML1（`copl/025.copl` - `copl/030.copl`）
- [ ] EvalML1Err（`copl/031.copl` - `copl/033.copl`）
- [ ] EvalML2（`copl/034.copl` - `copl/039.copl`）

完了条件:
- 3 game とも checker 実装 + fixture 全通。

### M3: ML 評価の拡張層（束縛表現含む）

目的: 評価規則が増える層と nameless 系を段階的に導入する。

対象:
- [ ] EvalML3（`copl/040.copl` - `copl/053.copl`）
- [ ] NamelessML3（`copl/054.copl`, `056.copl`, `058.copl`, `060.copl`, `062.copl`, `064.copl`, `066.copl`, `068.copl`）
- [ ] EvalNamelessML3（`copl/055.copl`, `057.copl`, `059.copl`, `061.copl`, `063.copl`, `065.copl`, `067.copl`, `069.copl`）

完了条件:
- 3 game とも checker 実装 + fixture 全通。

### M4: 追加評価系（ML4/5）

目的: ML 評価系列の後段を実装する。

対象:
- [ ] EvalML4（`copl/070.copl` - `copl/077.copl`）
- [ ] EvalML5（`copl/078.copl`, `copl/079.copl`）

完了条件:
- 2 game とも checker 実装 + fixture 全通。

### M5: 型付け系

目的: ファイル番号順（`080` 以降）で typing 系を実装し、診断（RuleViolation）の品質を揃える。

対象:
- [ ] TypingML4（`copl/080.copl` - `copl/106.copl`）
- [ ] PolyTypingML4（`copl/107.copl` - `copl/123.copl`）

完了条件:
- 2 game とも checker 実装 + fixture 全通。

### M6: 継続系

目的: ファイル番号順で最後に継続ルール群を実装する。

対象:
- [ ] EvalContML1（`copl/124.copl` - `copl/129.copl`）
- [ ] EvalContML4（`copl/130.copl` - `copl/140.copl`）

完了条件:
- 2 game とも checker 実装 + fixture 全通。

## 横断タスク

- [ ] game 追加の都度 `README.md` の対応 game 一覧を更新する。
- [ ] game 追加の都度 `docs/design.md` の実装状況を更新する。
- [ ] game 追加の都度 `docs/PLAN.md` と本計画の進捗を更新する。
- [ ] ルール適用エラーメッセージの expected/actual/fix 形式を全 game で揃える。

## 検証ゲート

各マイルストン完了時に以下を実行する:

- `cargo fmt`
- `cargo test`
- `cargo clippy --all-targets --all-features -- -D warnings`

## 次の着手候補

1. M1 の `EvalNatExp` から着手（最小ルールから TDD 開始）。
2. `copl/` 番号昇順を崩さず、`ReduceNatExp` へ進む。
