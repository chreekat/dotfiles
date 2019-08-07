# ~/.bashrc: Options for interactive shells.

shopt -s histappend autocd cdspell checkhash checkjobs globstar
# TODO
#direxpand       off
#dirspell        off
#dotglob         off
#execfail        off
#expand_aliases  on
#extdebug        off
#extglob         on
#extquote        on
#failglob        off
#force_fignore   on
#globasciiranges off
#gnu_errfmt      off
#histappend      on
#histreedit      off
#histverify      off
#hostcomplete    off
#huponexit       off
#inherit_errexit off
#interactive_comments    on
#lastpipe        off
#lithist         off
#login_shell     off
#mailwarn        off
#no_empty_cmd_completion off
#nocaseglob      off
#nocasematch     off
#nullglob        off
#progcomp        on
#promptvars      on
#restricted_shell        off
#shift_verbose   off
#sourcepath      on
#xpg_echo        off

export LESS=-iRFX
export PAGER=less
export MP_FULLNAME="Bryan Thomas Richter"
export MPW_FULLNAME="Bryan Thomas Richter"
export HISTCONTROL=ignoreboth
export HISTSIZE=-1
export HISTTIMEFORMAT="│%F %k:%M:%S│ "
for i in "$HOME/bin" "$HOME/.local/bin"; do
    if [ -d "$i" ] ; then
        PATH="$i:$PATH"
    fi
done
#export PS0='\e[2;31m[\#/\! @ \t]\e[0m\n'
export FIGNORE=*.o:*.hi
stty -ixon

# Source aux settings in ~/.bash
for i in $HOME/.bash/*.bash; do
    . $i
done

# Running dircolors with defaults is, for some reason, different than not
# running dircolors.
. <(dircolors)

## ALIASES + FUNCTIONS

alias f=fg
alias g=git
alias j=jobs
alias jb=jobs
alias la='ls -a'
alias n.='ls -ad .*'
alias n=ls
alias ns=nix-shell
alias pd=pushd
alias rg='rg --colors path:fg:cyan --colors path:style:bold'
alias up='g st'
alias xo=xdg-open
alias xp='xclip -selection clipboard'

# Pretty ripgrep, with less
prg () { rg -p $@ | less; }

# ripgrep to vim
qrg () { vim -q <(rg --vimgrep "$@"); }

md () {
    mkdir -p $1 && cd $1
}
