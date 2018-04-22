shopt -s histappend
export LESS=-iRFX
export PAGER=less
export MPW_FULLNAME="Bryan Thomas Richter"
export HISTCONTROL=ignoreboth
export HISTSIZE=-1
export HISTFILESIZE=10000
export HISTTIMEFORMAT="│%F %k:%M:%S│ "
export PATH="~/.local/bin:$PATH"
export PS0='\e[2;31m[\#/\! @ \t]\e[0m\n'
export FIGNORE=*.o:*.hi
stty -ixon
. ~/.bash/git.bash

## ALIASES

alias f=fg
alias g=git
alias jb=jobs
alias pd=pushd
alias xo=xdg-open
alias xp='xclip -selection clipboard'

# Pretty ripgrep, with less
prg () { rg -p $@ | less; }

# Oft-used
md () {
    mkdir -p $1 && cd $1
}
