# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ ./hardware-configuration.nix
      ../user-b.nix
      ../server-sudo.nix
      ../server-ssh.nix
    ];

  system.stateVersion = "22.11";

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/vda";
  };

  networking.hostName = "puny";

  # Required to use ACME
  security.acme.defaults.email = "b@chreekat.net";
  security.acme.acceptTerms = true;

  networking.firewall.allowedTCPPorts = [
    22
    80
    443
  ];
  services.nginx.enable = true;
  services.nginx.virtualHosts."puny.chreekat.net" = {
    forceSSL = true;
    enableACME = true;
    root = "/var/www/puny";
  };
}
