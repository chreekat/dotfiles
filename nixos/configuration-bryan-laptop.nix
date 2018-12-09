{ config, pkgs, ... }:

{
  networking.hostName = "bryan-laptop";
  system.stateVersion = "18.09";

  imports =
    [ /etc/nixos/hardware-configuration.nix
      /etc/nixos/system-common.nix
    ];

  # LUKS
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/sda2";
      preLVM = true;
    }
  ];
}
