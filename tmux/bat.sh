#!/usr/bin/env bash

acSym=''
if [ "$(cat /sys/class/power_supply/AC/online)" = 1 ]; then
    acSym='+'
else
    acSym='-'
fi

echo -n "$acSym"

cat \
    /sys/class/power_supply/BAT0/charge_full \
    /sys/class/power_supply/BAT0/charge_now \
    <(echo 100*r/n) \
    | dc

echo %
