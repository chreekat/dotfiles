#
# hledger timetracking stuff
#

export TIMELOG=~/.timeclock

to () {
    if ( tail -n 1 $TIMELOG | grep -q ^i )
    then
        echo o `date "+%Y-%m-%d %H:%M:%S"` >> $TIMELOG
    fi
}

ti () {
    if ( tail -n 1 $TIMELOG | grep -q ^i )
    then
        to
    fi
    # First argument is account; two spaces separate it from the description.
    # Hence the "" arg, which makes the spaces happen.
    echo i `date "+%Y-%m-%d %H:%M:%S"` $1 ${2:+""} ${@:2} >> $TIMELOG
}

alias tck='hledger -f $TIMELOG'
alias tck-week='hledger -f $TIMELOG bal -D --begin "this week"'
