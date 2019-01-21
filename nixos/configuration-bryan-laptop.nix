{ config, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
    /etc/nixos/system-common.nix
  ];

  # LUKS
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/sda2";
      preLVM = true;
    }
  ];

  environment.systemPackages = with pkgs; [
    ansible
  ];

  networking.hostName = "bryan-laptop";
  networking.search = [ "relex.fi" "relexsolutions.com" ];

  users.users.b = {
    extraGroups = ["vboxusers" "docker"];
  };

  virtualisation = {
    virtualbox.host = {
      enable = true;
      headless = true;
    };
  };

  system.stateVersion = "18.09";

}
