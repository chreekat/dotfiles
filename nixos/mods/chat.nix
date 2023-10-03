{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    gnupg1compat
    keybase-gui
    mumble
    # cinny-desktop # Not available on 23.05 because it depends on end-of-life openssl
    element-desktop
    signal-desktop
    wire-desktop
  ];
}
