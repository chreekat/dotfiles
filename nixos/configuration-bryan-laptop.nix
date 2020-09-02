{ config, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
    ./system-common.nix
    ./mats-vpnc-service.nix
    ./isengard-integration-tests.nix
  ];

  # Need all ram for building...
  boot.tmpOnTmpfs = false;

  # LUKS
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/sda2";
      preLVM = true;
    };
  };

  networking = {
    hostName = "bryan-laptop";
    search = [ "relex.fi" "relexsolutions.com" ];
  };

  # RELEX cache
  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
      "s3://isengard-store?region=eu-central-1"
    ];
    trustedBinaryCaches = [
      "http://cache.mordor.relexsolutions.com/"
      "s3://mordor-builder-cache?region=eu-central-1"
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
        gateway = "gp-vpn.relex.fi";
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
