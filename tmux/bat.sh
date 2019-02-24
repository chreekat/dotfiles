#!/usr/bin/env bash

datum () {
    echo "/sys/class/power_supply/BAT0/${1}_${2}"
}

sys_datum () {
    if [ -f "$(datum charge full)" ]; then
        datum charge $1
    else
        datum energy $1
    fi
}

cat "$(sys_datum full)" "$(sys_datum now)" <(echo 100*r/p) | dc
