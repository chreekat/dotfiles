{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.xserver.windowManager.notion;
  unstable = import (
    pkgs.fetchzip {
      url = "https://github.com/NixOS/nixpkgs/archive/0f5ce2fac0c726036ca69a5524c59a49e2973dd4.zip";
      sha256 = "0nkk492aa7pr0d30vv1aw192wc16wpa1j02925pldc09s9m9i0r3";
    }) {};
in

{
  disabledModules = ["services/x11/window-managers/notion.nix"];
  options = {
    services.xserver.windowManager.notion.enable = mkEnableOption "notion";
  };

  config = mkIf cfg.enable {
    services.xserver.windowManager = {
      session = [{
        name = "notion";
        start = ''
          ${unstable.pkgs.notion}/bin/notion &
          waitPID=$!
        '';
      }];
    };
    environment.systemPackages = [ unstable.pkgs.notion ];
  };
}
