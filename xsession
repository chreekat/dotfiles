configure_X () {
    /home/b/bin/scr on
    xsetroot -mod 16 16 -bg "#262626"
    autorandr -c
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
    xcape # Backgrounds itself

    notion
}

configure_X
case ${1:-unused} in
    config)
        ;;
    *) run_initial_apps
        ;;
esac
