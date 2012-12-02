#!/bin/bash

set -e

doo () {
    $@
}

errm () {
    2>&1 echo -e "$@"
}

confirm_no_clobber() {
    NOTADOT=''

    for i in ${DOTS[@]}; do
        if [ ! -L $i -a -e $i ]; then
            NOTADOT="${NOTADOT}$i "
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
            NOFINDINGS="${NOFINDINGS}$i"
        # Searchable if directory?
        elif [ -d "$goody" -a ! -x "$goody" ]; then
            NOFINDINGS="${NOFINDINGS}$i"
        fi
    done

    if [ -n "$NOFINDINGS" ]; then
        errm "\n  ABORT\n\n  These goodies don't exist in a state we can use!"
        errm "    $NOFINDINGS"
        exit 2
    fi
}

link_dot() {
    dot=$1
    doo rm $dot
    doo ln -s $EXPORT_DIR/$dot .
}

main () {
    confirm_no_clobber
    confirm_have_goodies
    for i in ${DOTS[@]}; do
        link_dot $i
    done
}

EXPORT_DIR=`dirname $0`
DOTS=(
    .dircolors
    .cvsignore
    .bash_aliases
    .ssh
    .ackrc
    .toprc
    .nethackrc
    .vim
    .vimrc
    .irssi
    .gitconfig
)

cd $HOME
main
