{ config, pkgs, lib, ... }:
{
  environment.systemPackages = [
    pkgs.light # backlight setter
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
  #     kpress("XF86MonBrightnessDown", "notioncore.exec('light -U 1')"),
  #     bdoc("Brightness up", ""),
  #     kpress("XF86MonBrightnessUp", "notioncore.exec('light -A 1')")
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
  # Allow autorandr to retry more aggressively when displays take a moment to settle.
  systemd.services.autorandr.startLimitBurst = lib.mkForce 5;
  systemd.services.autorandr.serviceConfig.ExecStart =
    let cfg = config.services.autorandr;
    in
      lib.mkForce ''
        ${pkgs.autorandr}/bin/autorandr \
        --batch \
        --change \
        --default ${cfg.defaultTarget} \
        ${lib.optionalString cfg.ignoreLid "--ignore-lid"} \
        ${lib.optionalString cfg.matchEdid "--match-edid"} ;
        sleep 1
      '';
}
