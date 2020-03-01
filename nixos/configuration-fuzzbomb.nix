{ config, pkgs, ... }:

let
  statefulness = {
    # LUKS is where root and swap hide.
    boot.initrd.luks.devices.crypted.device = "/dev/disk/by-uuid/a201e00a-e97b-4539-bc9b-462bba2570c6";

    networking.hostName = "fuzzbomb";

    system.stateVersion = "17.03";

  };
  imports = [
    /etc/nixos/hardware-configuration.nix
    ./system-common.nix
    ./games.nix
  ];
in
statefulness // {
  inherit imports;

  hardware.cpu.intel.updateMicrocode = true;

  services.transmission = {
    enable = true;
    settings = {
      ratio-limit-enabled = true;
      ratio-limit = 2.5;
    };
  };

  users.users.b.extraGroups = ["docker" "transmission"];

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };
}
