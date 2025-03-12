basic () {
    hledger -f timedot:Log.txt "$@"
}

bal () {
    basic bal --sort "$@"
}

dd () {
    date -d "$@" +%F
}

# Change s:unbilled to s:billed, but only for entries earlier than this week. We
# know when this week starts because it has a date line equal to *or greater than
# this week's Monday*.
mark_billed () {
    local tmp
    tmp=$(mktemp)
    trap 'rm -f "$tmp"' EXIT

    local this_monday
    if [[ $(date +%A) = Monday ]]; then
        this_monday=$(date +%F)
    else
        this_monday=$(dd last-monday)
    fi
    replace=true

    while IFS= read -r line; do
        if [[ $line =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2} ]]; then
            if [[ ! $line < $this_monday ]]; then
                replace=false
            fi
        fi

        if $replace; then
            echo "''${line//s:unbilled/s:billed}"
        else
            echo "$line"
        fi
    done < Log.txt > "$tmp"
    mv "$tmp" Log.txt
    bal --pivot s
}

if [[ ! -f Log.txt ]]; then
    echo "Log.txt not found in current directory."
    exit 1
fi

case $1 in
    # Are there any entries that don't have a status? That would be bad!
    check)
        basic reg not:tag:s=. amt:'>0'
        ;;

    # Status as determined by the 's' tag
    status)
        bal --pivot s
        ;;

    # Unbilled entries that need to go into the next invoice.
    next)
        echo "All:"
        bal tag:s=unbilled

        echo
        echo "Ending last week (to be invoiced):"
        bal tag:s=unbilled --end 'this week'
        ;;

    # Mark an invoice as sent
    invoice)
        # Do nothing if there are uncommitted changes to Log.txt
        if ! git diff -q --exit-code Log.txt; then
            echo "Uncommitted changes to Log.txt"
            exit 1
        fi
        mark_billed
        ;;

    *)
        basic "$@"
        ;;
esac
