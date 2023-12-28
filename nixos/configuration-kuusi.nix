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
      ./mods/haskell-foundation.nix
    ];

  # (NOTE: Copied from fuzzbomb, values tweaked)
  # Balance cores and max-jobs experimentally.
  # Watching
  # https://discourse.nixos.org/t/are-there-concrete-suggestions-for-balancing-cores-and-max-jobs/11824
  # for new ideas.
  nix.settings.cores = 4;
  nix.settings.max-jobs = 4;

  services.openssh.enable = true;
  services.openssh.listenAddresses = [ { addr = tailscaleIP; port = 22; } ];

  services.logind.lidSwitchExternalPower = "ignore";
}

