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
# How about no
unset LESSOPEN LESSKEYIN_SYSTEM
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
export FIGNORE="*.o:*.hi"
stty -ixon

# Source aux settings in ~/.bash
for i in "$HOME"/.bash/*.bash; do
    # shellcheck disable=SC1090 # Can't make this work anyway
    . "$i"
done

# Tweak dircolors defaults where they don't work.
# All the 00;90s don't work in Solarized light; switch to 38;5;244
LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=00:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.7z=01;31:*.ace=01;31:*.alz=01;31:*.apk=01;31:*.arc=01;31:*.arj=01;31:*.bz=01;31:*.bz2=01;31:*.cab=01;31:*.cpio=01;31:*.crate=01;31:*.deb=01;31:*.drpm=01;31:*.dwm=01;31:*.dz=01;31:*.ear=01;31:*.egg=01;31:*.esd=01;31:*.gz=01;31:*.jar=01;31:*.lha=01;31:*.lrz=01;31:*.lz=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.lzo=01;31:*.pyz=01;31:*.rar=01;31:*.rpm=01;31:*.rz=01;31:*.sar=01;31:*.swm=01;31:*.t7z=01;31:*.tar=01;31:*.taz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tgz=01;31:*.tlz=01;31:*.txz=01;31:*.tz=01;31:*.tzo=01;31:*.tzst=01;31:*.udeb=01;31:*.war=01;31:*.whl=01;31:*.wim=01;31:*.xz=01;31:*.z=01;31:*.zip=01;31:*.zoo=01;31:*.zst=01;31:*.avif=01;35:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:*~=38;5;244:*#=38;5;244:*.bak=38;5;244:*.crdownload=38;5;244:*.dpkg-dist=38;5;244:*.dpkg-new=38;5;244:*.dpkg-old=38;5;244:*.dpkg-tmp=38;5;244:*.old=38;5;244:*.orig=38;5;244:*.part=38;5;244:*.rej=38;5;244:*.rpmnew=38;5;244:*.rpmorig=38;5;244:*.rpmsave=38;5;244:*.swp=38;5;244:*.tmp=38;5;244:*.ucf-dist=38;5;244:*.ucf-new=38;5;244:*.ucf-old=38;5;244:';
export LS_COLORS

## ALIASES + FUNCTIONS

alias cal='cal -3mw'
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
alias sc=systemctl
alias usc='systemctl --user'
alias tree='tree -I dist-newstyle'
alias autorandr='autorandr --match-edid'

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
    local account action
    for arg in account action; do
        eval declare "$arg"="$1"
        shift
    done
    case "$account" in
        relex)
            declare pass_key='keepass-relex'
            declare kdbx_path=~/Syncthing/RelexFiles/relex-keepass.kdbx
            ;;
        personal)
            declare pass_key='keepass-chreekat'
            declare kdbx_path=~/Syncthing/PhoneFiles/chreekat.kdbx
            ;;
        *)
            >&2 echo "Unknown account: $account"
            return 1
            ;;
    esac

    case "$action" in
        search|clip)
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

rpl () { __pass_keepass relex search "$1"; }
rpc () { __pass_keepass relex clip "$1"; }
rtotp () { __pass_keepass relex totp "$1"; }

# Unlock keypass for personal
ppl () { __pass_keepass personal search "$1"; }
ppc () { __pass_keepass personal clip "$1"; }
ptotp () { __pass_keepass personal totp "$1"; }

# Pretty ripgrep, with less
prg () { rg -p "$@" | less; }

# ripgrep to vim
qrg () { vim -q <(rg --vimgrep "$@"); }

# git grep to vim
qgg () { vim -q <(git grep --recurse-submodules "$@"); }

mkcd () {
    set -e
    mkdir -p "$1" && cd "$1"
}

nixos () {
    set -e
    cd ~/Projects/dotfiles/nixos/
}

# The struggle is real
:q () {
    logout
}

eval "$(direnv hook bash)"
