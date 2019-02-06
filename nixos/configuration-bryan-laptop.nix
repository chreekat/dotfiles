{ config, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
    ./system-common.nix
    /home/b/RELEX/the-repo/support/mordor-local-dev.module.nix
    ./relex-vpn.nix
  ];

  # LUKS
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/sda2";
      preLVM = true;
    }
  ];

  environment.systemPackages = with pkgs; [
    ansible
  ];

  networking.hostName = "bryan-laptop";
  networking.search = [ "relex.fi" "relexsolutions.com" ];

  relex.developer = "b";
  relex.vpn.username = "bryan.richter";

  system.stateVersion = "18.09";
}
