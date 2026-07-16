#!/usr/bin/env bash

bat=/sys/class/power_supply/BAT0

read_attr () {
    cat "$bat/$1" 2>/dev/null
}

is_num () {
    [[ "$1" =~ ^[0-9]+$ ]]
}

status=$(read_attr status)
energy_now=$(read_attr energy_now)
energy_full=$(read_attr energy_full)
power_now=$(read_attr power_now)

# Prefer the energy counters for fill percentage; fall back to the coarse
# capacity attribute on batteries that don't expose them.
if is_num "$energy_now" && is_num "$energy_full" && ((energy_full > 0)); then
    capacity=$((energy_now * 100 / energy_full))
else
    capacity=$(read_attr capacity)
fi

case "$status" in
    Full)
        charging="="
        ;;
    Charging)
        charging="+"
        remaining=$((energy_full - energy_now))
        ;;
    Discharging)
        charging="-"
        remaining=$energy_now
        ;;
    "Not charging")
        charging="↯"
        ;;
    *)
        charging="?"
        ;;
esac

# Smooth the instantaneous power draw with an exponential moving average kept in
# a state file, so the ETA doesn't jump around with momentary load spikes. The
# average is reset when the reading is stale or the charge direction flipped.
state=${XDG_RUNTIME_DIR:-/tmp}/tmux-bat.state
alpha=30 # percent weight given to the newest sample

power=$power_now
if is_num "$power_now"; then
    now=$(date +%s)
    if [[ -r "$state" ]]; then
        read -r prev_power prev_status prev_time <"$state"
        if is_num "$prev_power" && is_num "$prev_time" \
            && [[ "$prev_status" == "$status" ]] && ((now - prev_time <= 120)); then
            power=$(((alpha * power_now + (100 - alpha) * prev_power) / 100))
        fi
    fi
    printf '%s %s %s\n' "$power" "$status" "$now" >"$state"
fi

# H:MM until the battery reaches full (charging) or empty (discharging), from
# the smoothed draw. Empty when power draw is unknown or zero (e.g. idle).
eta=""
if is_num "$remaining" && is_num "$power" && ((power > 0)); then
    eta=$(awk -v e="$remaining" -v p="$power" 'BEGIN {
        h = e / p
        hh = int(h)
        mm = int((h - hh) * 60 + 0.5)
        if (mm == 60) { hh++; mm = 0 }
        printf " %dh%02dm", hh, mm
    }')
fi

echo "$charging$capacity%$eta"
