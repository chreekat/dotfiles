{ config, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
    /etc/nixos/system-common.nix
    /home/b/RELEX/the-repo/support/mordor-local-dev.module.nix
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

  system.stateVersion = "18.09";
}
