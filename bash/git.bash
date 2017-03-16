# A collection of git aliases. Pushed into a proper script to allow
# variable reuse. (And hey, four less characters to type.)

log_aliases () {
    local gl='git log --date=short --graph'

    local single_line='%C(6)%h%Creset %s%C(auto).%d %C(4)%cd, %an% GK%Creset'

    local multi_line='%C(6)│       │%n%C(6)│%h│%Creset %s%C(auto).%d%C(4) %cd, %an% GK%n%C(6)│       │%Creset'

    declare -A aliases
    aliases[lg]="$gl --first-parent --pretty=tformat:'$single_line'"
    aliases[la]="$gl --all          --pretty=tformat:'$single_line'"
    aliases[lgs]="${aliases[lg]} --stat"
    aliases[las]="${aliases[la]} --stat"
    aliases[lg1]="${aliases[lg]} | head -n 20"
    aliases[la1]="${aliases[la]} | head -n 20"
    aliases[lgp]="$gl -p --first-parent --pretty=tformat:'$multi_line'"
    aliases[lap]="$gl -p --all          --pretty=tformat:'$multi_line'"

    for a in ${!aliases[@]}
    do
        alias $a="${aliases[$a]}"
    done
}

log_aliases

alias regit="source ~/.bash/git.bash"
alias cim="git ci -m"

