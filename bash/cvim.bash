_cvim() {
    COMPREPLY=()
    local cur="${COMP_WORDS[COMP_CWORD]}"
    local changed
    changed="$(git diff --name-only HEAD 2>/dev/null; git ls-files --others --exclude-standard 2>/dev/null)"
    [ -z "$changed" ] && return
    COMPREPLY=( $(compgen -W "$changed" -- "$cur") )
}

cvim() { vim "$@"; }

complete -F _cvim cvim
