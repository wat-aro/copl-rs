# Nat Checker 実装計画（履歴）

最終更新日: 2026-02-15
状態: 完了（履歴化）

## 目的

- `Nat` checker の立ち上げと、次ゲーム追加に耐える最小アーキテクチャの確立。

## 主要成果

- `Nat` / `CompareNat1` / `CompareNat2` / `CompareNat3` checker を実装。
- `copl-rs checker --game <name> [file]` の CLI 契約を実装。
- game 追加ガイド（`docs/how-to-add-a-game.md`）を整備。

## 主要な設計判断

- 汎用導出木 + checker 側 rule 解決（ADR-0005）。
- failing node の `SourceSpan` を含む診断（ADR-0006）。
- `NatJudgment` の明示 enum 表現（ADR-0007）。
- 成功時は root judgment text を出力（ADR-0008）。

## 注記

- 旧詳細計画は `docs/PLAN.md` 統合時に要約し、本ファイルを履歴として保持する。
