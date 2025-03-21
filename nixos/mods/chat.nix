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
}
