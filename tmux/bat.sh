#!/usr/bin/env bash

cat \
    /sys/class/power_supply/BAT0/charge_full \
    /sys/class/power_supply/BAT0/charge_now \
    <(echo 100*r/p) \
    | dc
