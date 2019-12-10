{ config, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
    ./system-common.nix
    ./mats-vpnc-service.nix
  ];

  # Need all ram for building...
  boot.tmpOnTmpfs = false;

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

  # RELEX cache
  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
    ];
    trustedBinaryCaches = [
      "http://cache.mordor.relexsolutions.com/"
      "s3://isengard-store?region=eu-central-1"
    ];
    binaryCachePublicKeys = [
      "nixcache.devs.relexsolutions.com-1:PRveyTUC6M1NGXo4Dg29CXsdc+KQOPPa7bRoXeLgGyI="
    ];
  };

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
        gateway = "gp-vpn2.relex.fi";
        id = "linux";
        secret = import ./vpnc-secret;
        username = "bryan.richter@relexsolutions.com";
      };
    };
  };

  system.stateVersion = "18.09";

  users.users.b.extraGroups = ["docker" "vboxusers"];

  virtualisation = {
    # For running Degustacion
    docker = {
      enable = true;
      enableOnBoot = false;
    };
    # For local environments
    virtualbox = {
      host.enable = true;
      host.headless = true;
    };
  };
}
