#!/usr/bin/env bash

set -Eeuo pipefail

# Utility functions

## For ease of iterative experimentation
doo () {
    $@
    # echo $@
}

# This function was originally named errm to be short for "error message", but
# then I realized that it sounds like a person saying, "Errm, excuse me, I don't
# think that's what you meant to do."
errm () {
    2>&1 echo -e "$@"
}

# START HERE.
main () {
    cd $HOME
    confirm_no_clobber
    confirm_have_goodies
    for i in ${DOTS[@]}; do
        link_dot $i
    done
    # TODO: Make sure permissions are legit. .ssh and .ghci, I'm lookin at you.

    start_user_units
}

# Subroutines
confirm_no_clobber() {
    NOTADOT=''

    for i in ${DOTS[@]}; do
        dst=.$i
        if [ ! -L $dst -a -e $dst ]; then
            NOTADOT="${NOTADOT}$dst "
        fi
    done

    if [ -n "$NOTADOT" ]; then
        errm "\n  ABORT"
        errm "\n  These exist but are not symlinks:"
        errm "    $NOTADOT"
        exit 2
    fi
}

confirm_have_goodies() {
    NOFINDINGS=''

    if [ ! -e "$EXPORT_DIR" ]; then
        errm "\n  ABORT\n\n  Where ya gonna copy them files from again?"
        errm "    Couldn't find export dir: '$EXPORT_DIR'"
        exit 2
    fi

    for i in ${DOTS[@]}; do
        goody="$EXPORT_DIR/$i"
        # Exists, is readable?
        if [ ! -r "$goody" ]; then
            NOFINDINGS="${NOFINDINGS}$i "
        # Searchable if directory?
        elif [ -d "$goody" -a ! -x "$goody" ]; then
            NOFINDINGS="${NOFINDINGS}$i "
        fi
    done

    if [ -n "$NOFINDINGS" ]; then
        errm "\n  ABORT\n\n  These goodies don't exist in a state we can use!"
        errm "    $NOFINDINGS"
        exit 2
    fi
}

link_dot() {
    src=$1
    dst=.$1
    doo rm -f "$dst"
    doo mkdir -p $(dirname "$dst")
    doo ln -s $EXPORT_DIR/$src $dst
}

start_user_units () {
    systemctl --user enable --now lorri.socket
}

# Initialize globals
EXPORT_DIR=$(dirname "${PWD}/$0")
DOTS=(

    Xresources
    ackrc
    bash
    bash_login
    bashrc
    config/astroid
    config/git/config
    config/systemd/user/lorri.service
    config/systemd/user/lorri.socket
    config/urxvt/themes/apprentice
    config/urxvt/themes/solarized-light
    ctags
    cvsrc
    ghci
    gnupg/gpg-agent.conf
    ignore
    inputrc
    irssi
    jshintrc
    mailcap
    mutt
    muttrc
    nethackrc
    notion/cfg_notion.lua
    notion/cfg_notioncore.lua
    notion/look.lua
    notion/notion-lock
    notmuch-config
    npmrc
    offlineimap
    offlineimaprc
    terminfo
    tmux
    tmux.conf
    vim
    vimrc
    w3m
    weechat
    xmonad
    xsession

)

# Fire missiles
main
