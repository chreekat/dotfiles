# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  imports =
    [ ./hardware-configuration.nix
    ];

  system.stateVersion = "22.11";

  boot.loader.grub = {
    enable = true;
    device = "/dev/vda";
  };

  networking.hostName = "puny";
  networking.domain = "chreekat.net";

  services.www-fileserv = {
    enable = true;
    admin-email = "b@chreekat.net";
  };
  security.acme.acceptTerms = true;

}
