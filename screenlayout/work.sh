#!/bin/sh
xrandr --output DP1-1 --mode 1920x1200 --pos 2560x0 --rotate normal
sleep 1
xrandr --output DP1-8 --primary --mode 2560x1440 --pos 0x0 --rotate normal
sleep 1
xrandr --output eDP1 --off
