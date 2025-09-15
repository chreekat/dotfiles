__reset_prompt () {
    last_status=$?
    if [[ "$last_status" -ne 0 ]]; then
    echo -e "🟥 ${RED}Last exit code: $last_status${RESET}"
    fi

    local color;
    case "$HOSTNAME" in
        kuusi)
            color=32
            ;;
        *)
            color=37
            ;;

    esac

    local path;
    path="$(__prompt_path)"

    PS1="\n${IN_NIX_SHELL:+∅ }\[\033[1;${color}m\][\[\e]0;\u@\h: \a\]\u@\h:$path]\$\[\033[0m\] "
}

__prompt_path() {
  {
  local cwd repo_paths=() path_parts=() p

  cwd=$(pwd)

  # Traverse up the path, collecting git roots
  p="$cwd"
  while [[ "$p" != "/" ]]; do
    if [[ -d "$p/.git" || -f "$p/.git" ]]; then
      repo_paths=("$p" "${repo_paths[@]}")
    fi
    p=$(dirname "$p")
  done

  # If no git roots, just return path
  if [[ ${#repo_paths[@]} -eq 0 ]]; then
    echo -e "\[\033[1;31m\]$cwd\[\033[0m\]"
    return
  fi

  # Now split path and style by boundaries
  local last="${repo_paths[0]}"
  local base="${cwd#$last}"
  local out="\[\033[37m\]$last"

  for ((i=1; i<${#repo_paths[@]}; i++)); do
    local r="${repo_paths[$i]}"
    local seg="${r#$last}"
    out+="\[\033[0m\]\[\033[36m\]$seg"
    last="$r"
  done

  # Remaining path inside innermost repo
  local inner="${cwd#$last}"
  out+="\[\033[0m\]\[\033[1;32m\]$inner\[\033[0m\]"

  echo -e "$out"
  } | sed "s#$HOME#~#"
}

# Use in your PS1
export PS1='\[\033[1;32m\][\u@\h:$(prompt_path)]\$\[\033[0m\] '

PROMPT_COMMAND=__reset_prompt
