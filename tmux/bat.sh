#!/usr/bin/env bash

datum () {
    echo "/sys/class/power_supply/BAT0/${1}_${2}"
}

status=$(cat /sys/class/power_supply/BAT0/status)
capacity=$(cat /sys/class/power_supply/BAT0/capacity)

case "$status" in
    Full)
        charging="="
        ;;
    Charging)
        charging="+"
        ;;
    Discharging)
        charging="-"
        ;;
    "Not charging")
        charging="↯"
        ;;
    *)
        charging="?"
        ;;
esac

echo "$charging$capacity%"
