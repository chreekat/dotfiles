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
    ./mods/laptop.nix
  ];
in
statefulness // {
  inherit imports;

  hardware.cpu.intel.updateMicrocode = true;

  # Built from channels rather than systems/flake.nix, so the sources the
  # shared modules expect from the flake are supplied by hand.
  _module.args.inputs = {
    nonfree-fonts = ./overlays/nonfree-fonts;
    agenix-cli = builtins.fetchTarball "https://github.com/ryantm/agenix/archive/main.tar.gz";
    losh-t = fetchGit { url = "https://github.com/sjl/t"; };
  };

  system.autoUpgrade = {
    enable = true;
    dates = "12:30";
  };

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

  # Balance cores and max-jobs experimentally.
  # Watching
  # https://discourse.nixos.org/t/are-there-concrete-suggestions-for-balancing-cores-and-max-jobs/11824
  # for new ideas.
  nix.settings.cores = 2;
  nix.settings.max-jobs = 2;

  # Treat this device as a "server"
  services.logind.lidSwitch = "ignore";
  services.openssh.listenAddresses = [ { addr = tailscaleIP; } ];
}
