{ pkgs, lib, config, ...} :
let
  xkb-reload = pkgs.writeShellApplication {
    name = "xkb-reload";
    runtimeInputs = [ pkgs.xorg.setxkbmap pkgs.xorg.xkbcomp ];
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
      thunar # File browser
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

  services.unclutter-xfixes.enable = true;

  services.urxvtd = {
    enable = false;
    package = pkgs.rxvt-unicode.override (drv: {
      rxvt-unicode-unwrapped = pkgs.rxvt-unicode-unwrapped.overrideAttrs (orig: {
        patches = orig.patches ++ [
          ../patches/urxvt-garbage.patch
        ];
      });
    });
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
        autorandrCmd = lib.concatStringsSep " " [
          "${pkgs.autorandr}/bin/autorandr"
          "--batch"
          "--change"
          "--default default"
          "--match-edid"
        ];
        lockerWrapper = pkgs.writeShellScript "xsecurelock-with-autorandr" ''
          # When locked with DPMS suspend, monitor unplug events may be missed.
          # This watcher triggers autorandr on DPMS wake so the login prompt
          # appears on the correct screen before authentication.
          (
            prev_state=""
            while true; do
              if ${pkgs.xorg.xset}/bin/xset q 2>/dev/null \
                   | ${pkgs.gnugrep}/bin/grep -q "Monitor is On"; then
                cur_state="On"
              else
                cur_state="Off"
              fi
              if [ "$cur_state" = "On" ] && [ -n "$prev_state" ] && [ "$prev_state" != "On" ]; then
                sleep 2
                ${autorandrCmd}
              fi
              prev_state="$cur_state"
              sleep 1
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

          ${autorandrCmd}
        '';
      in
        "${lockerWrapper}";
  };
}
