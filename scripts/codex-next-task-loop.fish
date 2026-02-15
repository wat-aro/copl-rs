#!/usr/bin/env fish

# Run Codex with a fixed prompt repeatedly.
# Stop when Codex exits with a non-zero code.

set -l no_task_token "__CODEX_NO_TASK__"
set -l pending_token "__CODEX_PENDING__"
set -l prompt "次のタスクに取り掛かって。着手前に AGENTS.md の設計原則（YAGNI/KISS/SOLID）に照らして妥当性を判断し、妥当でない場合は pending として実行せずタスクを終了して $pending_token とだけ出力して。次のタスクがない場合は $no_task_token とだけ出力して終了して"
set -l count 1

while true
    echo "[run $count] codex: $prompt"
    set -l output_file (mktemp)
    codex exec "$prompt" | tee $output_file
    set -l status_code $pipestatus[1]

    if test $status_code -ne 0
        echo "codex exited with $status_code. stopping loop."
        rm -f $output_file
        break
    end

    if rg -q --fixed-strings "$no_task_token" $output_file
        echo "no next task detected. stopping loop."
        rm -f $output_file
        break
    end

    if rg -q --fixed-strings "$pending_token" $output_file
        echo "task closed as pending. continuing to next task."
    end

    rm -f $output_file
    set count (math $count + 1)
end
