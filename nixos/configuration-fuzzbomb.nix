{ config, pkgs, ... }:

let
  tailscaleIP = "100.92.232.15";
  statefulness = {
    networking.hostName = "fuzzbomb";
    system.stateVersion = "17.03";
  };
  imports = [
    /etc/nixos/hardware-configuration.nix
    ./system-common.nix
  ];
in
statefulness // {
  inherit imports;

  hardware.cpu.intel.updateMicrocode = true;

  #services.transmission = {
  #  enable = true;
  #  settings = {
  #    ratio-limit-enabled = true;
  #    ratio-limit = 2.5;
  #  };
  #};

  environment.systemPackages = [
    pkgs.tor-browser-bundle-bin
  ];

  users.users.b.extraGroups = ["docker" "transmission"];

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  # Balance cores and max-jobs experimentally.
  # Watching
  # https://discourse.nixos.org/t/are-there-concrete-suggestions-for-balancing-cores-and-max-jobs/11824
  # for new ideas.
  nix.buildCores = 2;
  nix.maxJobs = 2;

  services.openssh.listenAddresses = [ { addr = tailscaleIP; } ];
}
