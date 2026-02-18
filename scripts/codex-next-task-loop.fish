#!/usr/bin/env fish

# Run Codex with a fixed prompt repeatedly.
# Stop when Codex exits with a non-zero code.

argparse h/help -- $argv
or exit 2

if set -q _flag_help
    echo "Usage: codex-next-task-loop.fish"
    echo "Runs each iteration in a new Codex session."
    exit 0
end

set -l no_task_token "__CODEX_NO_TASK__"
set -l prompt "次のタスクに取り掛かって。次のタスクがない場合は $no_task_token とだけ出力して終了して"
set -l count 1

while true
    echo "[run $count] codex (new session): $prompt"
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

    rm -f $output_file
    set count (math $count + 1)
end
