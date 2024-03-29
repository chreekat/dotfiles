#!/usr/bin/env bash
set -Eeuo pipefail

usage='# Usage

    ./chg_brightness.sh [up | down]
'
help="
chg_brightness.sh

Changes screen brightness via the sys filesystem at
/sys/class/backlight/

Also requires udev rules: see
https://wiki.archlinux.org/index.php/backlight#ACPI

$usage

"

min () {
    if [[ $1 -lt $2 ]]
    then
        echo $1
    else
        echo $2
    fi
}

max () {
    if [[ $1 -gt $2 ]]
    then
        echo $1
    else
        echo $2
    fi
}

calc_new_brightness () {
    dir=${1:?}
    max=${2:?}
    cur=${3:?}

    let "incr = max / 20" \
        "min = incr" \
        "stepUp = cur + incr" \
        "stepDown = cur - incr"

    case $dir in
        up)
            min $stepUp $max
            ;;
        down)
            max $stepDown $min
            ;;
    esac
}

main () {
    direction=${1?$'missing parameter.\n\n'"$usage"}

    device=$(find /sys/class/backlight -type l)
    control_file="$device/brightness"
    max=$(cat $device/max_brightness)
    cur=$(cat $control_file)

    echo $(calc_new_brightness $direction $max $cur) | tee $control_file
}

main $@
