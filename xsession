configure_X () {
    # Blank screen after 3 minutes, dim for 10, and then really blank the
    # screen. This can't be done through NixOS yet: the two ways of doing it are
    # xset (procedural) and arguments to Xserver itself.
    #
    # Also, the dim timeout needs to be shared with the locker command in
    # nixos/system-common.nix.
    xset s 180 10
    xset s blank
    xsetroot -mod 16 16 -bg "#262626"
}

run_initial_apps () {
    # These are services! :o Add them as local services!)

    flameshot &
    xcape # Backgrounds itself

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
