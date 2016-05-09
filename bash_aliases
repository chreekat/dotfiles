#!/bin/bash

alias n="LC_COLLATE=C ls"
alias n.="n -ad .*"
alias na="n -a"
alias nal="n -al"
alias alrt="n -alrt"
alias ack=ack-grep
alias thes="dict -d moby-thesaurus"
alias randmusic="play -R"
alias totrandmusic="find ~/Music -name '*.mp3' | mpg123 -Z@ -"
alias tpb="/usr/lib/surfraw/piratebay"
alias g="/usr/lib/surfraw/google"
alias ataction="mutt -f =@action"
alias f="fg"
alias j="jobs"
alias pd='pushd'
alias df='df -h'
alias o='offlineimap -q -o -u basic -f INBOX -k mbnames:enabled=false'

alias action_lists="riot ~/Org/Lists.riot"
alias amv='audio_rename -p "Music/%a/%l/%{mA}%{n0}__%t" -R'

# Hopefully, I will never understand TERM.
alias tmux="TERM=screen-it tmux"

# For safety
alias apt-get="echo 'Use aptitude'"

alias uu="sudo aptitude update && sudo aptitude -y safe-upgrade"

alias t='python ~/src/t/t.py --task-dir ~/.todo --list todo'
alias may='python ~/src/t/t.py --task-dir ~/.todo --list maybe'

alias ds="aptitude search"

alias dsh="aptitude show"
alias di="sudo aptitude install"
_pkgs () {
  # from /usr/share/bash-completion/completions/aptitude
  COMPREPLY=( $( apt-cache pkgnames $2 2> /dev/null ) )
}
complete -F _pkgs dsh
complete -F _pkgs di
complete -F _pkgs ds

alias gd="git d"

alias git=hub

alias xo=">/dev/null 2>&1 xdg-open"

alias gdiff="vim +Gdiff"

alias htag="git ls-tree -r HEAD --name-only | grep -E '*.hs' | xargs fast-tags"
alias ag='ag --color --color-line-number=36 --color-path=32 --color-match="90;45" --pager less $@'

alias IN="vim -S ~/.vim/IN.in.vim ~/IN.in"

alias git-week='lgf --since=`date +%Y-%m-%d -d "last week"` --author=chreekat'
