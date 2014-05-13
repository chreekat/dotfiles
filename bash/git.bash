# A collection of git aliases. Pushed into a proper script to allow
# variable reuse. (And hey, four less characters to type.)

log_aliases () {
    gl='git log --date=short'
    line='%C(10)%h │%Creset %s%C(auto)%d%C(10) │ %cd, %an%Creset'
    std_format="--pretty=tformat:'$line'"
    big_break='%C(13 bold)********************************************************************************%C(reset)%n'
    break_format="--pretty=tformat:'$big_break$line'"
    almost_all="--branches --remotes" # But not tags



    alias lg="$gl     --graph $almost_all --first-parent        $std_format"
    alias lgs="$gl    --graph $almost_all --first-parent --stat $std_format"
    alias lgf="$gl    --graph $almost_all                       $std_format"
    alias lga="$gl    --graph             --first-parent        $std_format"
    alias lp="$gl  -p                     --first-parent        $break_format"
    alias lpf="$gl -p                                           $break_format"

    alias lg1="$gl    --graph $almost_all --first-parent        $std_format | head -n 10"
}

log_aliases

alias pl="git pull --ff-only"

br-clone () {
  repo=$1
  shift
  git clone $repo -o barobo -c author.email=bryan@barobo.com $@
}
