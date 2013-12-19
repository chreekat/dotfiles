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
alias o='offlineimap -qf INBOX'

alias action_lists="riot ~/Org/Lists.riot"
alias amv='audio_rename -p "Music/%a/%l/%{mA}%{n0}__%t" -R'

#  TODO: ignore binary files?
alias vimdiffem="vim \$(git status -s | awk '/^.M/ { print \$2 }')"

# Hopefully, I will never understand TERM.
alias tmux="TERM=screen-it tmux"

# For safety
alias apt-get="echo 'Use aptitude'"

alias uu="sudo aptitude update && sudo aptitude -y safe-upgrade"

alias t='python ~/src/t/t.py --task-dir ~/.todo --list todo'
alias may='python ~/src/t/t.py --task-dir ~/.todo --list maybe'
