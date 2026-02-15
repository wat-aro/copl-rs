#!/usr/bin/env fish

# Run Codex with a fixed prompt repeatedly.
# Stop when Codex exits with a non-zero code.

argparse h/help s/session= -- $argv
or exit 2

if set -q _flag_help
    echo "Usage: codex-next-task-loop.fish [--session <id-or-thread>]"
    echo
    echo "  --session  Reuse the specified Codex session for every iteration."
    echo "             If omitted, resume the most recent session (--last)."
    exit 0
end

set -l no_task_token "__CODEX_NO_TASK__"
set -l prompt "次のタスクに取り掛かって。次のタスクがない場合は $no_task_token とだけ出力して終了して"
set -l count 1

set -l session ""
if set -q _flag_session
    set session $_flag_session[1]
end

while true
    if test -n "$session"
        echo "[run $count] codex (session=$session): $prompt"
    else
        echo "[run $count] codex: $prompt"
    end
    set -l output_file (mktemp)
    if test -n "$session"
        codex exec resume "$session" "$prompt" | tee $output_file
    else
        codex exec resume --last "$prompt" | tee $output_file
    end
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
