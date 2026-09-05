#!/usr/bin/env bash
# Status line for Claude Code, mirroring the shell prompt structure:
# [user@host:path] | model | context%

input=$(cat)

cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd // empty')
# Abbreviate home directory
cwd="${cwd/#$HOME/~}"

model=$(echo "$input" | jq -r '.model.display_name // empty')

remaining=$(echo "$input" | jq -r '.context_window.remaining_percentage // empty')

# Publish the session name for the notify hook, which only gets a session_id
session_id=$(echo "$input" | jq -r '.session_id // empty')
session_name=$(echo "$input" | jq -r '.session_name // empty')
if [ -n "$session_id" ] && [ -n "$session_name" ]; then
    name_file="${XDG_CACHE_HOME:-$HOME/.cache}/claude-notify/name-$session_id"
    if [ "$(cat "$name_file" 2>/dev/null)" != "$session_name" ]; then
        mkdir -p "${name_file%/*}"
        printf '%s' "$session_name" > "$name_file"
    fi
fi

# Build [user@host:path] section in bold green (color 32), matching prompt style
host=$(hostname -s)
user=$(whoami)

parts="\033[1;32m[${user}@${host}:${cwd}]\033[0m"

# Append model name
[ -n "$model" ] && parts="$parts \033[37m${model}\033[0m"

# Append context remaining percentage when available
[ -n "$remaining" ] && parts="$parts \033[37mctx:$(printf '%.0f' "$remaining")%\033[0m"

printf "%b" "$parts"
