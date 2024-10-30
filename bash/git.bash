# A collection of git aliases. Pushed into a proper script to allow
# variable reuse. (And hey, four less characters to type.)

log_aliases () {
    local gitlog='git log --date=short --graph --color=always'

    local single_line='%C(reverse red)%h%C(auto) %s.%d %C(4)%cd, %an%C(auto) %GK'

    local multi_line='%C(reverse red)│ %h │%C(auto) %s. (%C(auto)%D%C(reset)) %C(4)%cd, %an%C(auto) (%GK %GS)'

    declare -A aliases
    aliases[gl]="$gitlog  --first-parent --pretty=tformat:'$single_line'"
    aliases[gla]="$gitlog --all          --pretty=tformat:'$single_line'"
    aliases[glf]="$gitlog                --pretty=tformat:'$single_line'"
    aliases[gls]="${aliases[gl]}   --stat"
    aliases[glas]="${aliases[gla]} --stat"
    aliases[glfs]="${aliases[glf]} --stat"
    aliases[gl1]="${aliases[gl]}   | head -n 20"
    aliases[gla1]="${aliases[gla]} | head -n 20"
    aliases[glp]="$gitlog  -p --first-parent --pretty=tformat:'$multi_line'"
    aliases[glap]="$gitlog -p --all          --pretty=tformat:'$multi_line'"
    aliases[glfp]="$gitlog -p                --pretty=tformat:'$multi_line'"

    for a in "${!aliases[@]}"
    do
        # shellcheck disable=SC2139 # I want it.
        alias "$a"="${aliases[$a]}"
    done
}

log_aliases

alias regit="source ~/.bash/git.bash"

alias gcd='git ci -m `ddate -1`'
