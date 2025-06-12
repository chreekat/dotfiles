{ config, pkgs, ... }:
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
}
