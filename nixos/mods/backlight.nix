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
}

# Added to Notion's cfg_bindings.lua:
#
# defbindings("WScreen", {
#     bdoc("Brightness down", ""),
#     kpress("XF86MonBrightnessDown", "notioncore.exec('light -U 1')"),
#     bdoc("Brightness up", ""),
#     kpress("XF86MonBrightnessUp", "notioncore.exec('light -A 1')")
# }
