#!/usr/bin/env bash
set -Eeuo pipefail

usage='# Usage

    ./intel_brightness.sh [up | down]
'
help="
intel_brightness.sh

Changes screen brightness via the sys filesystem at
/sys/class/backlight/intel_backlight.

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

    control_file=/sys/class/backlight/intel_backlight/brightness
    max=$(cat /sys/class/backlight/intel_backlight/max_brightness)
    cur=$(cat $control_file)

    echo $(calc_new_brightness $direction $max $cur) | tee $control_file
}

main $@
