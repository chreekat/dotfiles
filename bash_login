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
export HISTFILESIZE=1000000
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

## ALIASES

alias f=fg
alias g=git
complete -o bashdefault -o default -o nospace -F __git_wrap__git_main g
alias la='ls -a'
alias jb=jobs
alias pd=pushd
alias rg='rg --colors path:fg:cyan --colors path:style:bold'
alias xo=xdg-open
alias xp='xclip -selection clipboard'
alias j=jobs

# Dvorak
alias n=ls
alias n.='ls -ad .*'

# Pretty ripgrep, with less
prg () { rg -p $@ | less; }

# Oft-used
md () {
    mkdir -p $1 && cd $1
}

### Use fzf completion and key bindings
## Use ~~ as the trigger sequence instead of the default **
#export FZF_COMPLETION_TRIGGER='~~'
## Options to fzf command
#export FZF_COMPLETION_OPTS='+c -x'
## Use fd (https://github.com/sharkdp/fd) instead of the default find
## command for listing path candidates.
## - The first argument to the function ($1) is the base path to start traversal
## - See the source code (completion.{bash,zsh}) for the details.
#_fzf_compgen_path() {
#  fd --hidden --follow . "$1"
#}
## Use fd to generate the list for directory completion
#_fzf_compgen_dir() {
#  fd --type d --hidden --follow . "$1"
#}
#source `fzf-share`/completion.bash
#source `fzf-share`/key-bindings.bash
