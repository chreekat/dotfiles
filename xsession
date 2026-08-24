# The X session does not inherit systemd's user environment, where the gcr
# agent's socket is advertised.
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/gcr/ssh"

configure_X () {
    /home/b/bin/scr on
    xsetroot -mod 16 16 -bg "#262626"
}

run_initial_apps () {
    # These are services! :o Add them as local services!)

    notify-osd&
    wmsystemtray&
    sleep 0.5

    nm-applet&
    flameshot &
    cbatticon &
    wmCalClock &
    keynav # Backgrounds itself

    notion
}

configure_X
case ${1:-unused} in
    config)
        ;;
    *) run_initial_apps
        ;;
esac
