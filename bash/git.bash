# A collection of git aliases. Pushed into a proper script to allow
# variable reuse. (And hey, four less characters to type.)

log_aliases () {
    local gitlog='git log --date=short --graph'

    local single_line='%C(6)%h%Creset %s%C(auto).%d %C(4)%cd, %an% GK%Creset'

    local multi_line='%C(6)│       │%n%C(6)│%h│%Creset %s%C(auto).%d%C(4) %cd, %an% GK%n%C(6)│       │%Creset'

    declare -A aliases
    aliases[gl]="$gitlog --first-parent --pretty=tformat:'$single_line'"
    aliases[gla]="$gitlog --all          --pretty=tformat:'$single_line'"
    aliases[gls]="${aliases[lg]} --stat"
    aliases[glas]="${aliases[la]} --stat"
    aliases[gl1]="${aliases[lg]} | head -n 20"
    aliases[gla1]="${aliases[la]} | head -n 20"
    aliases[glp]="$gitlog  -p --first-parent --pretty=tformat:'$multi_line'"
    aliases[glap]="$gitlog -p --all          --pretty=tformat:'$multi_line'"

    for a in ${!aliases[@]}
    do
        alias $a="${aliases[$a]}"
    done
}

log_aliases

alias regit="source ~/.bash/git.bash"
