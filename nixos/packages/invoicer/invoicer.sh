usage () {
    echo "Usage: invoicer [-a|--archived] [check|status [period]|next|invoice [month|all]|<arbitrary hledger command line>]"
    echo "Example:    invoicer status '3 weeks ago last Monday'"
}

basic () {
    local file_args=()
    for f in "${FILES[@]}"; do
        file_args+=(-f "timedot:$f")
    done

    hledger "${file_args[@]}" "$@"
}

bal () {
    basic bal --sort "$@"
}

dd () {
    date -d "$@" +%F
}

check () {
    output=$(basic reg not:tag:s=. amt:'>0')
    if [[ -n $output ]]; then
        echo "$output"
        return 1
    fi
}

# Change s:unbilled to s:billed, but only for entries earlier than the current
# period (month or all).
mark_billed () {
    # Do nothing if there are uncommitted changes to Log.txt
    if ! git diff -q --exit-code "${FILES[@]}"; then
        echo "Uncommitted changes to ${FILES[*]}."
        exit 1
    elif ! check >&/dev/null; then
        echo "Check failed!"
        check
        exit 1
    fi

    local this_period has_cutoff=true
    if [[ $1 = month ]]; then
        this_period=$(date +%Y-%m-01)
    elif [[ $1 = all ]]; then
        has_cutoff=false
    else
        echo "Invalid period: $1"
        exit 1
    fi

    local tmp
    tmp=$(mktemp)

    # shellcheck disable=SC2064
    trap "rm -f '$tmp'" EXIT

    for file in "${FILES[@]}"; do
        local replace=true
        while IFS= read -r line; do
            if [[ $line =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2} ]]; then
                if "$has_cutoff" && [[ ! $line < $this_period ]]; then
                    replace=false
                fi
            fi

            if $replace; then
                echo "${line//s:unbilled/s:billed}"
            else
                echo "$line"
            fi
        done < "$file" > "$tmp"
        mv "$tmp" "$file"
    done

    bal --pivot s
}

USE_ARCHIVED=false
FILES=( Log.txt )

while :; do
    case ${1:-} in
        -a|--archived)
            USE_ARCHIVED=true
            shift ;;
        --help)
            usage
            exit 0
            ;;
        --) shift; break ;;
        -*)
            echo "Unknown option: $1" >&2;
            usage
            exit 1
            ;;
        *) break ;;
    esac
done

if [[ ! -f Log.txt ]]; then
    echo "Log.txt not found in current directory."
    exit 1
fi

if $USE_ARCHIVED; then
    FILES+=( Log_*.txt )
fi

case ${1:-} in
    # Are there any entries that don't have a status? That would be bad!
    check)
        check
        ;;

    # Status as determined by the 's' tag
    status)
        period=${2:-'3 weeks ago last Monday'}
        bal --pivot s:acct --tree --sort -WT -b "$(dd "$period")"
        ;;

    # Unbilled entries that need to go into the next invoice.
    next)
        echo "All unbilled:"
        bal tag:s=unbilled

        echo
        echo -n "Ending last month: "
        date +%B -d 'last month'
        bal tag:s=unbilled --end 'this month'
        ;;

    # Create invoice and mark entries as billed
    invoice)
        period=${2:-month}
        contract=$(basename "$PWD")
        month=$(date +%Y-%m -d 'last month')
        invoice-create "$contract" "$month"
        mark_billed "$period"
        ;;

    ""|help)
        usage
        ;;

    *)
        basic "$@"
        ;;
esac
