# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=100000
HISTFILESIZE=100000
HISTTIMEFORMAT="│%F %k:%M:%S│ "

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

stty -ixon

## FUNCTIONS

md () {
    mkdir -p $@ &&
    cd $@
}

# Probably something is non-normalized here:
export LESS="-RFX"
export PAGER=less

export FIGNORE=*.o:*.hi

export PARINIT='rTbgqR B=.,?_A_a Q=_s>|'

for rc in ~/.bash/*.bash; do
    source $rc
done
