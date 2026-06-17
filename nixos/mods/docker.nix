{ pkgs, ... }:
{
  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };
  environment.systemPackages = [
    pkgs.docker-credential-helpers
  ];
}
