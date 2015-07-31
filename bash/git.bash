# A collection of git aliases. Pushed into a proper script to allow
# variable reuse. (And hey, four less characters to type.)

log_aliases () {
    gl='git log --color=always --date=short'
    line='%C(10)%h │%G? %GK│%Creset %C(4)%s%Creset%C(auto).%d%C(10) %cd, %an%Creset'
    std_format="--pretty=tformat:'$line'"
    big_break='%C(13 bold)********************************************************************************%C(reset)%n'
    break_format="--pretty=tformat:'$big_break$line'"



    alias lg="$gl     --graph --branches --first-parent --remotes       $std_format"
    alias lgs="$gl    --graph --branches --first-parent --remotes --stat $std_format"
    alias lgf="$gl    --graph --branches         --remotes              $std_format"
    alias lgsff="$gl   --graph --branches         --remotes       --stat $std_format"
    alias lgaa="$gl    --graph             --first-parent                 $std_format"
    alias lgaff="$gl   --graph                                            $std_format"
    alias lp="$gl  -p                     --first-parent                 $break_format"
    alias lpff="$gl -p                                                    $break_format"
    alias lgg="lg | head -n 20"
    alias lgss="lgs | head -n 20"
    alias lgff="lgf | head -n 20"
    alias lgsf="lgsff | head -n 20"
    alias lga="lgaa | head -n 20"
    alias lgaf="lgaff | head -n 20"
    alias lpp="lp | head -n 20"
    alias lpf="lpff | head -n 20"

    alias lg1="$gl    --graph --branches --first-parent  --remotes      $std_format | head -n 10"
}

log_aliases

# FIXME wtf is this? Did I write this?
addsubrepo () {
  git submodule add $(git -C $1 remote -v | cut -f2| grep -v fetch | cut -f1 -d' ')
}

alias regit="source ~/.bash/git.bash"
alias cim="git ci -m"

