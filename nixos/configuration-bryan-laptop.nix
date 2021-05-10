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

  environment.systemPackages = [
    pkgs.ansible
    pkgs.teams
    (import /home/b/RELEX/repos/hparg {}).hparg
  ];

  networking = {
    hostName = "bryan-laptop";
    search = [ "relex.fi" "relexsolutions.com" ];
    firewall.allowedUDPPorts = [
      # 3478-3481 for Teams
      3478
      3479
      3480
      3481
    ];
  };

  # RELEX cache
  nix = {
    binaryCachePublicKeys = [
      "mordor.gitlab-ci:qYaMiGpGkUfs3aQVpLJgvB45NMp+wcaoUkiB4tw8sy4="
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
