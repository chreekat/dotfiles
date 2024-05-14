{ pkgs, lib, ...} :
{
  imports = [ ./kmonad.nix ];

  environment.systemPackages = with pkgs; [
    # Xorg (in concert with enabling xmonad)
      albert # Launcher, bound to ScrollLock
      arandr
      flameshot
      keynav
      notify-osd
      pavucontrol
      xfce.thunar # File browser
      xclip
      xorg.xev
      xorg.xmessage
    # Gifcasts (FIXME: make vokoscreen make gifs by default)
      screenkey # show keys in gif casts
      slop # Used by screenkey to select a region
      vokoscreen # gif casts
    # Xorg tray
      cbatticon
      networkmanagerapplet
  ];
  # Enable and configure the X11 windowing system.
  services.xserver = {
    enable = true;
    autoRepeatDelay = 300;
    autoRepeatInterval = 10;
    libinput.enable = true;
    windowManager.notion = {
      enable = true;
    };
    desktopManager.xfce = {
      enable = true;
    };

    ## X KEYBOARD MAP

    xkb.layout = "kmonad";

    # my snazzy config (not used anymore - using KMonad instead so dvorak only
    # applies to the internal keyboard).
    xkb.extraLayouts.b = {
      description = "Bryan's modified dvorak";
      languages = [ "eng" "swe" "fin" ];
      symbolsFile = pkgs.writeText "my-dvorak-symbols" ''
        xkb_symbols "b"  {
            include "pc+us(dvorak)+inet(evdev)"
            include "ctrl(nocaps)+compose(lctrl)+level3(ralt_switch)"

            key <AE04> { [ NoSymbol, NoSymbol, EuroSign, sterling ] };
            key <AD01> { [ NoSymbol, NoSymbol, aring, Aring ] };
            key <AD11> { [ NoSymbol, NoSymbol, dead_acute ] };
            key <AC01> { [ NoSymbol, NoSymbol, adiaeresis, Adiaeresis ] };
            key <AC02> { [ NoSymbol, NoSymbol, odiaeresis, Odiaeresis ] };
            key <AC03> { [ NoSymbol, NoSymbol, eacute, Eacute ] };
            key <AB01> { [ NoSymbol, NoSymbol, Greek_lambda, NoSymbol ] };
        };
      '';
    };
    # For use as a base for KMonad.
    #
    # I want the base underlying keyboard to be US, so that I can plug in
    # another keyboard like the Atreus and not have its output get twisted
    # through the Dvorak layer.
    #
    # At the same time, I want to handle certain keys via xkb, because there's
    # no way in KMonad to have Shift+RAlt+Quot equal RAlt+Shift+Quot (for upper
    # case Å) -- the modifiers don't easily commute.
    #
    # Since this is below KMonad, I have to specify the physical keys, e.g. e is
    # at AD03. That kinda sucks.
    xkb.extraLayouts.kmonad = {
      description = "KMonad base";
      languages = [ "eng" "swe" "fin" ];
      symbolsFile = pkgs.writeText "kmonad-base-symbols" ''
        xkb_symbols "kmonad"  {
            include "us(basic)"
            include "compose(rctrl)+level3(ralt_switch)"

            key <AE04> { [ NoSymbol, NoSymbol, EuroSign, sterling ] };
            key <AC11> { [ NoSymbol, NoSymbol, aring, Aring ] };
            key <AC01> { [ NoSymbol, NoSymbol, adiaeresis, Adiaeresis ] };
            key <AD09> { [ NoSymbol, NoSymbol, odiaeresis, Odiaeresis ] };
            key <AD03> { [ NoSymbol, NoSymbol, eacute, Eacute ] };
            key <AB08> { [ NoSymbol, NoSymbol, Greek_lambda, NoSymbol ] };
        };
      '';
    };
  };

  services.kmonad = {
    enable = true;
    package = pkgs.haskellPackages.kmonad;
    keyboards.lenovo = {
      name = "lenovo";
      device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
      # I want to set my special xkb layout manually, so no defcfg.
      defcfg.enable = false;
      config = lib.readFile ../lenovo-t14s.kbd;
    };
  };

  programs.xss-lock = {
    enable = true;
    extraOptions = [
      "--notifier=${pkgs.xsecurelock}/libexec/xsecurelock/dimmer"
      "--transfer-sleep-lock"
    ];
    lockerCommand =
      lib.concatStringsSep " " [
        "env XSECURELOCK_PASSWORD_PROMPT=disco"
        "    XSECURELOCK_BLANK_TIMEOUT=10"
        "    XSECURELOCK_BLANK_DPMS_STATE=off"
        # Need to escape the % because they get interpreted by systemd.
        "    XSECURELOCK_DATETIME_FORMAT='%%a %%d %%b %%Y, %%R %%Z, W%%V'"
        "    XSECURELOCK_SHOW_DATETIME=1"
        "${pkgs.xsecurelock}/bin/xsecurelock"
      ];
  };
}
