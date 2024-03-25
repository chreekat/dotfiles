__reset_prompt () {
    local color;
    case "$HOSTNAME" in
        kuusi)
            color=32
            ;;
        *)
            color=37
            ;;

    esac

    PS1="\n${IN_NIX_SHELL:+∅ }\[\033[1;${color}m\][\[\e]0;\u@\h: \w\a\]\u@\h:\w]\$\[\033[0m\] "
}

PROMPT_COMMAND=__reset_prompt
