configure_X () {
    # Blank screen after 3 minutes, dim for 5, and then really blank the screen.
    xset s 180 5
    xset s blank

    # Set up keyboard
    pkill xcape
    setxkbmap -option ctrl:nocaps
    xcape
    xset r rate 300 100
}

run_initial_apps () {
    # Run the flameshot service (it's a service! :o Add it as a local service!)
    flameshot &

    # So hawt
    xmonad
}

configure_X
case ${1:-unused} in
    config)
        ;;
    *) run_initial_apps
        ;;
esac
