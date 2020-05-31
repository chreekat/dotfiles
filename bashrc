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

alias cp='cp --reflink=auto'
alias f=fg
alias g=git
alias j=jobs
alias jb=jobs
alias la='ls -a'
alias n.='ls -ad .*'
alias n=ls
alias nb=nix-build
alias ns=nix-shell
alias pd=pushd
alias rg='rg --colors path:fg:cyan --colors path:style:bold'
alias txr=transmission-remote
alias up='g st'
alias xo=xdg-open
alias xp='xclip -selection clipboard'

#
# Fetch keys from keepass, via pass.
#
# "But why?" Because keepass works on Android and it supports TOTP, so it's a
# better source of truth. But pass has a better UI for me on Linux.

# Helper: Unlock keypass with pass
__pass_keepass_unlock () {
    declare acct="$1"
    shift
    pass show "$acct" | keepassxc-cli "$@"
}

# Helper: workhorse wrapper function
__pass_keepass () {
    for arg in account action; do
        eval declare "$arg"="$1"
        shift
    done
    case "$account" in
        relex)
            declare pass_key='keepass-relex'
            declare kdbx_path=~/GoogleDrive/RELEX/relex-keepass.kdbx
            ;;
        personal)
            declare pass_key='keepass-chreekat'
            declare kdbx_path=~/GoogleDrive/chreekat/chreekat.kdbx
            ;;
        *)
            >&2 echo "Unknown account: $account"
            return 1
            ;;
    esac

    case "$action" in
        locate|clip)
            __pass_keepass_unlock "$pass_key" "$action" "$kdbx_path" "$1"
            ;;
        totp)
            __pass_keepass_unlock "$pass_key" show -t "$kdbx_path" "$1" \
                | tail -n 1 | xclip -selection clipboard
            ;;
        *)
            >&2 echo "Unknown action: $action"
            return 1
            ;;
    esac

}

rpl () { __pass_keepass relex locate "$1"; }
rpc () { __pass_keepass relex clip "$1"; }
rtotp () { __pass_keepass relex totp "$1"; }

# Unlock keypass for personal
ppl () { __pass_keepass personal locate "$1"; }
ppc () { __pass_keepass personal clip "$1"; }
ptotp () { __pass_keepass personal totp "$1"; }

# Pretty ripgrep, with less
prg () { rg -p $@ | less; }

# ripgrep to vim
qrg () { vim -q <(rg --vimgrep "$@"); }

md () {
    mkdir -p $1 && cd $1
}


eval "$(direnv hook bash)"
