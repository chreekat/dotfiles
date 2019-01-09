{ config, pkgs, ... }:

{
  networking.hostName = "bryan-laptop";
  networking.search = [ "relex.fi" "relexsolutions.com" ];
  system.stateVersion = "18.09";

  imports =
    [ /etc/nixos/hardware-configuration.nix
      /etc/nixos/system-common.nix
      #/etc/nixos/relex-vpn.nix
    ];

  # LUKS
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/sda2";
      preLVM = true;
    }
  ];

  virtualisation = {
    virtualbox.host = {
      enable = true;
      headless = true;
    };
    docker.enable = true;
  };

  users.extraUsers.b = {
    extraGroups = ["vboxusers" "docker"];
  };

  # Relex VPN
  #networking.relexVpn = {
  #  enable = true;
  #  ipaUsername = "bryan.richter";
  #  sharedSecret = "psittacines";
  #};
}
