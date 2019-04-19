{ config, pkgs, ... }:

{
  networking.hostName = "fuzzbomb";
  system.stateVersion = "17.03";

  imports =
    [ /etc/nixos/hardware-configuration.nix
      ./system-common.nix
      ./steam.nix
    ];

  # LUKS is where root and swap hide.
  boot.initrd.luks.devices.crypted.device = "/dev/disk/by-uuid/a201e00a-e97b-4539-bc9b-462bba2570c6";

  hardware.cpu.intel.updateMicrocode = true;
}
