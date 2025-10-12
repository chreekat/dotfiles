{ config, pkgs, ... }:

let
  # FIXME make this a module!?
  tailscaleIP = "100.68.0.118";
  statefulness = {
    networking.hostName = "kuusi";
    system.stateVersion = "21.11";
  };
in statefulness // {
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      ./system-common.nix
      ./mods/laptop.nix
      ./mods/backlight.nix
      ./mods/games.nix
    ];

  # (NOTE: Copied from fuzzbomb, values tweaked)
  # Balance cores and max-jobs experimentally.
  # Watching
  # https://discourse.nixos.org/t/are-there-concrete-suggestions-for-balancing-cores-and-max-jobs/11824
  # for new ideas.
  nix.settings.cores = 4;
  nix.settings.max-jobs = 4;
  # Twice the default, since I got a warning to increase it.
  nix.settings.download-buffer-size = 2 * 67108864;

  services.openssh.enable = true;
  services.openssh.listenAddresses = [ { addr = tailscaleIP; port = 22; } ];

  services.logind.lidSwitchExternalPower = "ignore";

  # Kuusi has a nvme disk; this should make it faster.
  boot.initrd.luks.devices.root.bypassWorkqueues = true;
}

