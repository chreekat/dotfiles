# Blank screen after 3 minutes, dim for 5, and then really blank the screen.
xset s 180 5
xset s blank

# Set up keyboard
setxkbmap -option ctrl:nocaps
xcape
xset r rate 300 100

# Lock with xsecurelock/dimmer
xss-lock \
    --notifier=$(dirname $(readlink $(which xsecurelock)))/../libexec/xsecurelock/dimmer \
    --transfer-sleep-lock \
    -- xsecurelock &

# So hawt
urxvt -e tmux &
xmonad
