# A collection of git aliases. Pushed into a proper script to allow
# variable reuse. (And hey, four less characters to type.)

log_aliases () {
    local gl='git log --color=always --date=short --graph'

    local line='%C(10)%h │%Creset %s%C(auto).%d%C(10) %cd, %an% GK%Creset'
    local std_format="--pretty=tformat:'$line'"

    local big_break='%C(13 bold)********************************************************************************%C(reset)%n'
    local break_format="--pretty=tformat:'$big_break$line'"

    declare -A aliases
    aliases[lg]="$gl --first-parent $std_format"
    aliases[la]="$gl --all          $std_format"
    aliases[lgs]="${aliases[lg]} --stat"
    aliases[las]="${aliases[la]} --stat"
    aliases[lg1]="${aliases[lg]} | head -n 20"
    aliases[la1]="${aliases[la]} | head -n 20"

    for a in ${!aliases[@]}
    do
        alias $a="${aliases[$a]}"
    done
}

log_aliases

alias regit="source ~/.bash/git.bash"
alias cim="git ci -m"

