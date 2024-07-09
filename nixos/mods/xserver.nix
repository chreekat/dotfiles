{ pkgs, lib, ...} :
{
  console.useXkbConfig = true;
  environment.systemPackages = with pkgs; [
    # Xorg (in concert with enabling xmonad)
      arandr
      flameshot
      keynav
      notify-osd
      pavucontrol
      xfce.thunar # File browser
      xcape
      xclip
      xorg.xev
      xorg.xmessage
    # Gifcasts (FIXME: make vokoscreen make gifs by default)
      #screenkey # show keys in gif casts
      #slop # Used by screenkey to select a region
      vokoscreen-ng # gif casts
    # Xorg tray
      cbatticon
      networkmanagerapplet
  ];
  # Enable and configure the X11 windowing system.
  services.libinput.enable = true;
  services.xserver = {
    enable = true;
    autoRepeatDelay = 300;
    autoRepeatInterval = 10;
    windowManager.notion = {
      enable = true;
    };
    desktopManager.xfce = {
      enable = true;
    };

    ## X KEYBOARD MAP

    # Basic keyboard setup that gets reused by the console via
    # i18n.consoleUseXkbConfig.
    xkb.layout = "b";

    # my snazzy config.
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


    # NixOS' support for xkeyboard-config has a high impedance mismatch.
    # See the definition for rekey above.
    displayManager.sessionCommands = "rekey";
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
