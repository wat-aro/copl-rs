# Nat Checker 実装計画

最終更新日: 2026-02-06
このフェーズのスコープ: M2 実装中（Nat 構文とパーサまで着手済み）

## 目的

CoPL の `Nat` game 用 checker を Rust で実装する。
参照元:
- https://www.fos.kuis.kyoto-u.ac.jp/~igarashi/CoPL/games/Nat.html
- `copl/001.copl` から `copl/008.copl` で確認できる ASCII 記法

設計制約:
- 手続き的な分岐より、宣言的なモデル化を優先する。
- 仕様と不変条件は可能な限り型で表現する。
- 将来の game 追加に耐える拡張可能な構成にする。
- game の種類は CLI 引数で選択できるようにする。

## 進捗トラッカー

- [x] 共通の計画ファイルを作成する。
- [x] Nat の規則（`P-Zero`, `P-Succ`, `T-Zero`, `T-Succ`）と ASCII 入力例を確認する。
- [x] CLI の大枠方針（`copl-rs <subcommand> --game <name> <file>`）を合意する。
- [x] パーサ方針（手書き再帰下降）を合意する。
- [x] 厳格性方針（未知の規則名は即時エラー）を合意する。
- [x] エラー出力方針（まずはプレーンテキスト）を合意する。
- [x] 初期 crate 構成（1 crate）と `main` / `lib` 分離方針を合意する。
- [x] モジュール境界（`cli` / `core` / `games`、Nat は `syntax` / `parser` / `checker`）を合意する。
- [x] game レジストリ方式（`enum GameKind + match`）を合意する。
- [x] プロジェクトの足場（`Cargo.toml` と `src/` の初期構成）を作る。
- [x] Nat の項と判断を表す型付き AST を実装する。
- [ ] 規則ごとの型付き導出ノード（rule-indexed）を実装する。
- [x] Nat ASCII 記法のパーサを実装する。
- [ ] Nat 導出の checker 本体を実装する。
- [x] `copl-rs <subcommand> --game <name> [file]` の CLI を実装する。
- [x] 正常系フィクスチャ（`001.copl`-`008.copl`）のテストを追加する。
- [ ] 異常系テスト（規則不一致、木構造不正、結果項の不一致）を追加する。
- [ ] 新しい game を追加する手順を文書化する。

## 規則モデル（Nat）

対象の導出規則:
- `P-Zero`: `Z plus n is n`
- `P-Succ`: `n1 plus n2 is n` から `S(n1) plus n2 is S(n)` を導く
- `T-Zero`: `Z times n is Z`
- `T-Succ`: `n1 times n2 is n3` と `n2 plus n3 is n4` から `S(n1) times n2 is n4` を導く

想定する型表現:
- `NatTerm`: 再帰 enum（`Z | S(Box<NatTerm>)`）
- `NatJudgment`: `Plus` と `Times` の和型
- 規則ごとの証明ノード構造体（ペイロード未検査の巨大 enum を避ける）
- 解析済み構文を型付き証明木へ変換し、前提検査を宣言的に行う game 単位の trait

## 提案アーキテクチャ（将来の game 追加対応）

1. CLI 層:
   - 当面は `copl-rs checker --game <name> [file]` を解析する。
   - `[file]` 省略時は `stdin` を読む。
   - game レジストリにディスパッチする。
2. コアインターフェース:
   - `Game` trait（parser + checker + 表示フック）を定義する。
   - エラー/診断の共通型を定義する。
3. game モジュール:
   - `games/nat/` に Nat の構文・パーサ・checker・テストを配置する。
   - 以降の game も同じ境界で追加する。
4. テスト戦略:
   - `copl/*.copl` を正常系ゴールデンフィクスチャとして使う。
   - 規則ごとに異常系フィクスチャを用意する。

## マイルストン分解

### M1: 骨組みと契約
- crate / module 構成と `Game` trait を定義する。
- CLI のサブコマンド契約（まずは `checker`）と game 選択契約を追加する。
- 完了条件:
  - `copl-rs checker --game nat [file]` が Nat 実行経路へ到達する。

### M2: Nat 構文とパース
- CoPL ASCII 記法の項・判断・導出木を解析するパーサを実装する。
- 規則名（`P-Zero`, `P-Succ`, `T-Zero`, `T-Succ`）を保持する。
- 完了条件: `001.copl`-`008.copl` を型付き Nat 証明木へ変換できる。

### M3: Nat チェック
- 型付き前提から宣言的に規則検証を行う。
- 最初に失敗したノードを特定できる診断を返す。
- 完了条件: 正常系フィクスチャが全通、異常系フィクスチャが期待通り失敗する。

### M4: 拡張経路の整備
- 次ゲーム向けに再利用抽象を整理する。
- 新しい game モジュール追加手順を短いガイドにまとめる。
- 完了条件: 次ゲーム追加時に必要な変更が最小で、文書化されている。

## 次の議論で決める項目

- なし（実装着手可能）。

## 合意済み事項（2026-02-06）

- 実行バイナリ名は `copl-rs` とする。
- CLI はサブコマンド方式を採用する。
- 当面のサブコマンドは `checker` のみとする（`resolver` は将来追加）。
- game 指定は共通で `--game <name>` を使う。
- パーサは手書き再帰下降を採用する。
- 未知の規則名は即時エラーとする。
- エラー出力はまずプレーンテキストのみとする（JSON は後続検討）。
- 初期構成は 1 crate とし、`main` は CLI、ロジックは `src/lib.rs` に置く。
- モジュール境界は `cli` / `core` / `games` とし、`games/nat` は `syntax` / `parser` / `checker` に分ける。
- game レジストリは `enum GameKind + match` を採用する。
- 想定形:
  - `copl-rs checker --game nat <file>`
  - （将来）`copl-rs resolver --game nat <file>`

## 更新ルール

作業が進んだら、このファイルを都度更新する。
- チェックリストを `[ ]` から `[x]` へ更新する。
- `最終更新日` を更新する。
- 設計判断が変わった場合は、完了済みマイルストン配下に短い注記を残す。
