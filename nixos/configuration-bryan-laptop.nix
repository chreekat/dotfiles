{ config, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
    ./system-common.nix
    ./relex-vpn.nix
    ./mats-vpnc-service.nix
  ];

  # LUKS
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/sda2";
      preLVM = true;
    }
  ];

  networking = {
    hostName = "bryan-laptop";
    hosts = {
      # My PC environments
      "10.23.12.64" = [
        "bryan-legacy.relexsolutions.com"
        "bryan-scale-out.relexsolutions.com"
        "bryan-scale-out-nix.relexsolutions.com"
      ];
    };
    search = [ "relex.fi" "relexsolutions.com" ];
  };

  nixpkgs.overlays = [ (import ./relex-overlays.nix) ];

  relex.developer = "b";
  relex.vpn.username = "bryan.richter";

  services = {
    # RELEX policy
    clamav = {
      daemon.enable = true;
      daemon.extraConfig = ''
        ScanOnAccess yes
        OnAccessIncludePath /home/b/Downloads
      '';
      updater.enable = true;
    };
    vpnc.servers = {
      relex = {
        gateway = "gp-vpn.relex.fi";
        id = "linux";
        secret = import ./vpnc-secret;
        username = "bryan.richter@relexsolutions.com";
      };
    };
  };

  system.stateVersion = "18.09";

  users.users.b.extraGroups = ["docker"];

  # For running Degustacion
  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };
}
