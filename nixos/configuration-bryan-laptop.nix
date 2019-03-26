{ config, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
    ./system-common.nix
    /home/b/RELEX/the-repo/support/mordor-local-dev.module.nix
    ./relex-vpn.nix
    ./relex-specific.nix
  ];

  # LUKS
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/sda2";
      preLVM = true;
    }
  ];

  networking.hosts = {
    # My PC environments
    "10.23.12.64" = [
      "bryan-legacy.relexsolutions.com"
      "bryan-scale-out.relexsolutions.com"
      "bryan-scale-out-nix.relexsolutions.com"
    ];
  };
  networking.hostName = "bryan-laptop";
  networking.search = [ "relex.fi" "relexsolutions.com" ];

  nixpkgs.overlays = [ (import ./relex-overlays.nix) ];

  users.users.b.extraGroups = ["docker"];

  relex.developer = "b";
  relex.vpn.username = "bryan.richter";

  system.stateVersion = "18.09";

  # For running Degustacion
  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };
}
