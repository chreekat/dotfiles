#!/usr/bin/env bash

# The matching autorandr profile(s), or [NO DISPLAY] when nothing matches.
profiles=$(autorandr --current 2>/dev/null | paste -sd, -)

echo "${profiles:-[NO DISPLAY]}"
