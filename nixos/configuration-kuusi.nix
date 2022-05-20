{ config, pkgs, ... }:

let
  statefulness = {
    networking.hostName = "kuusi";
    system.stateVersion = "21.11";
    time.timeZone = "Europe/Helsinki";
  };
in statefulness // {
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      ./system-common.nix
    ];

  # (NOTE: Copied from fuzzbomb, values tweaked)
  # Balance cores and max-jobs experimentally.
  # Watching
  # https://discourse.nixos.org/t/are-there-concrete-suggestions-for-balancing-cores-and-max-jobs/11824
  # for new ideas.
  nix.buildCores = 4;
  nix.maxJobs = 4;
}

