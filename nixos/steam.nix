{ config, pkgs, ... }:

let
  unstable = import /home/b/src/nix/nixpkgs { config.allowUnfree = true; };
  #unstable = import (fetchGit {
  #  url = https://github.com/nixos/nixpkgs;
  #  ref = "master";
  #}) { config.allowUnfree = true; };
in
{
  environment.systemPackages = [ unstable.steam ];
  hardware = {
    opengl.driSupport32Bit = true;
    pulseaudio.support32Bit = true;
    steam-hardware.enable = true;
  };
  nixpkgs.config.allowUnfree = true;
}
