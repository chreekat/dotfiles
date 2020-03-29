{ config, lib, pkgs, ... }:
{
  disabledModules = [ "services/torrent/transmission.nix" ];

  imports = [
    ./transmission.nix
  ];

  services.transmission = {
    enable = true;
    settings = {
      ratio-limit-enabled = true;
      ratio-limit = 2.5;
    };
  };
}
