#!/usr/bin/env bash
set -Eeuo pipefail

usage='# Usage

    ./intel_brightness.sh [up | down]
'
help="
intel_brightness.sh

Changes screen brightness via the sys filesystem at
/sys/class/backlight/intel_backlight.

Requires perl. Also requires udev rules: see
https://wiki.archlinux.org/index.php/backlight#ACPI

$usage

"

calc_new_brightness () {
    dir=${1:?}
    max=${2:?}
    cur=${3:?}

    let "incr = ${max} / 20"

    calc=
    case $dir in
        up)
            calc="min($cur + $incr, $max)"
            ;;
        down)
            calc="max($cur - $incr, 0)"
            ;;
    esac
    perl -mList::Util=min,max -e "print $calc"
}

main () {
    direction=${1?$'missing parameter.\n\n'"$usage"}

    control_file=/sys/class/backlight/intel_backlight/brightness
    max=$(cat /sys/class/backlight/intel_backlight/max_brightness)
    cur=$(cat $control_file)

    echo $(calc_new_brightness $direction $max $cur) | tee $control_file
}

main $@
