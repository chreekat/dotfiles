#!/usr/bin/env bash

_git_worktree_paths() {
  git worktree list --porcelain | awk '
    /^worktree / {
      path = $2
      sub("\\.git/modules/", "", path)
      # Check if directory exists
      cmd = "test -d \"" path "\""
      if (system(cmd) == 0) {
        print path
      }
    }'
}

_git_wt_go_complete() {
  local cur="${COMP_WORDS[COMP_CWORD]}"
  COMPREPLY=( $(compgen -W "$(_git_worktree_paths)" -- "$cur") )
}

_git_wt_go_main() {
  local dir="$1"
  if [ -z "$dir" ]; then
    dir=$(_git_worktree_paths | fzf)
    [ -z "$dir" ] && return 1
  fi
  cd "$dir" || return 1
}

if [[ "${BASH_SOURCE[0]}" != "$0" ]]; then
  complete -F _git_wt_go_complete git-wtg
  git-wtg() { _git_wt_go_main "$@"; }
else
  echo "This script must be sourced, not run directly"
fi
