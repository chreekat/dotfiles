{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    element-desktop
    # cinny-desktop # Not available on 23.05 because it depends on end-of-life openssl
    discord
    gnupg1compat
    keybase-gui
    mumble
    signal-desktop
    slack
    telegram-desktop
    whatsapp-for-linux
    #wire-desktop # marked insecure 2024-10-23
  ];

  networking.hosts = {
    # Glowing Bear running on puny.
    "100.81.115.78" = [ "glowing-bear.chreekat" ];
  };
}
