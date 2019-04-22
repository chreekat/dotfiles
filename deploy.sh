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
    ISDIR=''

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
        # Is directory? We used to allow this, but now dem goodies is goin in
        # the nix store.
        elif [ -d "$goody" ]; then
            ISDIR="${ISDIR}$i "
        fi
    done

    if [ -n "$NOFINDINGS" ]; then
        errm "\n  ABORT\n\n  These goodies don't exist in a state we can use!"
        errm "    $NOFINDINGS"
        exit 2
    elif [ -n "$ISDIR" ]; then
        errm "\n  ABORT\n\n  These goodies are directories! Can't be having that!"
        errm "    $ISDIR"
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

# Initialize globals
EXPORT_DIR=@EXPORT_DIR@
DOTS=(
    ackrc
    bash/git.bash
    bash/haskell.bash
    bash/snowdrift.bash
    bash/vim-tag-completion.bash
    bash_login
    config/git/config
    config/urxvt/themes/apprentice
    config/urxvt/themes/solarized-light
    ctags
    cvsrc
    ghci
    ignore
    inputrc
    irssi/config
    irssi/default.theme
    irssi/scripts/autorun/cap_sasl.pl
    irssi/scripts/autorun/notify_me.pl
    irssi/scripts/cap_sasl-1.5.pl
    irssi/solarized-universal.theme
    jshintrc
    mailcap
    mutt/crypto.rc
    mutt/default-color.muttrc
    mutt/lunatic-mutt
    mutt/mailcap
    mutt/which_inbox
    muttrc
    nethackrc
    npmrc
    offlineimap/helper.py
    offlineimaprc
    profile
    terminfo/s/screen
    tmux/bat.sh
    tmux.conf
    toprc
    vim
    vimrc
    weechat
    w3m/keymap
    xmonad/chg_intel_brightness.sh
    xmonad/xmonad.hs
    xsession
    Xresources
)

# Fire missiles
main
