{ config, pkgs, lib, ... }:
{
  environment.systemPackages = [
    pkgs.brightnessctl # backlight setter
  ];

  # Allow the video group to change backlight brightness
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
    ACTION=="add", SUBSYSTEM=="backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
  '';

  # Added to Notion's cfg_bindings.lua:
  #
  # defbindings("WScreen", {
  #     bdoc("Brightness down", ""),
  #     kpress("XF86MonBrightnessDown", "notioncore.exec('brightnessctl set 1-')"),
  #     bdoc("Brightness up", ""),
  #     kpress("XF86MonBrightnessUp", "notioncore.exec('brightnessctl set +1')")
  # }

  location = {
    #provider = "geoclue2";
    provider = "manual";
    # Äkäslompolo
    #latitude = 67.6030203;
    #longitude = 24.17231;
    # Helsinki
    latitude = 60.2443;
    longitude = 24.8800;
    # Sintra
    #latitude = 38.8017;
    #longitude = -9.37979;
    # Zürich
    #latitude = 47.3745;
    #longitude = 8.5410;
  };

  services.tlp.enable = true;
  # Keep charge less than max when plugged in. Saves battery life.
  services.tlp.settings = {
    START_CHARGE_THRESH_BAT0 = 90;
    STOP_CHARGE_THRESH_BAT0 = 92;
  };

  # Prevent disaster; be useful
  services.logind.settings.Login.HandlePowerKey = "suspend";

  services.autorandr.enable = true;
  services.autorandr.matchEdid = true;
  environment.etc."xdg/autorandr/settings.ini".text = ''
    [config]
    match-edid = true
    skip-options = crtc
  '';
  # Some displays need a moment to settle after lid open / hot plug; re-trigger
  # autorandr.service a few times so the eventual stable configuration wins.
  # Done as a sibling service rather than an ExecStart override to stay robust
  # against upstream module changes.
  systemd.services.autorandr.startLimitBurst = lib.mkForce 20;
  systemd.services.autorandr-settle = {
    description = "Re-run autorandr to handle displays that settle slowly";
    after = [ "autorandr.service" ];
    wantedBy = [ "autorandr.service" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = pkgs.writeShellScript "autorandr-settle" ''
        for i in 3 3 4; do
          sleep $i
          ${pkgs.systemd}/bin/systemctl start autorandr.service || true
        done
      '';
    };
  };
}
