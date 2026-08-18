{ pkgs, lib, config, ...} :
let
  xkb-reload = pkgs.writeShellApplication {
    name = "xkb-reload";
    runtimeInputs = [ pkgs.setxkbmap pkgs.xkbcomp ];
    text = ''
      setxkbmap -print \
        | xkbcomp -I${config.services.xserver.xkb.dir} - "''${DISPLAY:?DISPLAY not set}"
    '';
  };
in
{
  console.useXkbConfig = true;
  environment.systemPackages = with pkgs; [
    # Xorg (in concert with enabling xmonad)
      arandr
      flameshot
      ghostty
      keynav
      notify-osd
      pavucontrol
      xclip
      xev
      xkb-reload
      xmessage
    # Gifcasts (FIXME: make vokoscreen make gifs by default)
      #screenkey # show keys in gif casts
      #slop # Used by screenkey to select a region
      vokoscreen-ng # gif casts
    # Xorg tray
      cbatticon
      networkmanagerapplet
  ];

  services.unclutter-xfixes = {
    enable = true;
    timeout = 5;
    threshold = 10;
  };

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
            include "level3(ralt_switch)"

            key <AE04> { [ NoSymbol, NoSymbol, EuroSign, sterling ] };
            key <AD01> { [ NoSymbol, NoSymbol, aring, Aring ] };
            key <AD11> { [ NoSymbol, NoSymbol, dead_acute ] };
            key <AC01> { [ NoSymbol, NoSymbol, adiaeresis, Adiaeresis ] };
            key <AC02> { [ NoSymbol, NoSymbol, odiaeresis, Odiaeresis ] };
            key <AC03> { [ NoSymbol, NoSymbol, eacute, Eacute ] };
            key <AB01> { [ NoSymbol, NoSymbol, Greek_lambda, NoSymbol ] };
            override key <MENU> { [ Multi_key ] };
        };
      '';
    };
  };
  programs.xss-lock = {
    enable = true;
    extraOptions = [
      "--notifier=${pkgs.xsecurelock}/libexec/xsecurelock/dimmer"
      "--transfer-sleep-lock"
    ];
    lockerCommand =
      let
        lockerWrapper = pkgs.writeShellScript "xsecurelock-locker" ''
          # The external's HPD toggle on self-standby makes X wake eDP's DPMS
          # behind xsecurelock's back; xsecurelock never re-blanks because it
          # only reacts to input. Re-assert the blank whenever the display is on
          # but no one is here. Gated on input idle so it never fights unlock,
          # and it never touches output config -- forcing DPMS off cannot loop.
          (
            while true; do
              if ${pkgs.xset}/bin/xset q 2>/dev/null \
                   | ${pkgs.gnugrep}/bin/grep -q "Monitor is On" \
                 && [ "$(${pkgs.xprintidle}/bin/xprintidle 2>/dev/null || echo 0)" -gt 12000 ]; then
                ${pkgs.xset}/bin/xset dpms force off
              fi
              sleep 2
            done
          ) &
          WATCHER_PID=$!
          trap "kill $WATCHER_PID 2>/dev/null" EXIT

          env XSECURELOCK_PASSWORD_PROMPT=disco \
              XSECURELOCK_BLANK_TIMEOUT=10 \
              XSECURELOCK_BLANK_DPMS_STATE=suspend \
              XSECURELOCK_DATETIME_FORMAT='%a %d %b %Y, %R %Z, W%V' \
              XSECURELOCK_SHOW_DATETIME=1 \
              ${pkgs.xsecurelock}/bin/xsecurelock

          # The external display drops its HPD line when it self-standbys during
          # lock, so X deconfigures it. Re-apply the profile once on unlock.
          ${pkgs.autorandr}/bin/autorandr --change
        '';
      in
        "${lockerWrapper}";
  };
  # Wait indefinitely for a fingerprint rather than dropping to the password
  # prompt after 30s. Set on the login stack, which xsecurelock authenticates
  # against by default (also TTY console login). max-tries is left at the
  # default, so repeated failed reads still fall through to the password.
  security.pam.services.login.rules.auth.fprintd.settings.timeout = -1;

  # A second pam_u2f attempt, identical to the first and still ahead of
  # fprintd. pam_u2f has no retry count of its own, so a second try means a
  # second stack entry.
  security.pam.services.login.rules.auth.u2f-retry =
    let
      u2f = config.security.pam.services.login.rules.auth.u2f;
    in
      {
        inherit (u2f) enable control modulePath settings;
        order = u2f.order + 1;
      };
}
