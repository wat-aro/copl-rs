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

### 現在進行中の計画はありません

- 次の計画は backlog 起票後にこの節へ追加する。

## 履歴計画

- 完了または凍結した計画は `docs/plans/` に保存する。
- このファイルには現在の計画のみを保持する。
- 履歴一覧:
  - [2026-02-20 Prover 実装計画（Nat-first）完了アーカイブ](plans/2026-02-20-prover-roadmap-completed.md)
  - [2026-02-19 Prover 実装計画 完了タスクアーカイブ（01-17）](plans/2026-02-19-prover-roadmap-completed-01-17.md)
  - [2026-02-16 Checker 実装ロードマップ（完了）](plans/2026-02-16-checker-roadmap-completed.md)
  - [2026-02-15 Checker 実装ロードマップ](plans/2026-02-15-checker-roadmap.md)
  - [2026-02-14 Nat Checker 実装計画](plans/2026-02-14-nat-checker.md)
