{ config, pkgs, ... }:

{
  boot.extraModprobeConfig = "options kvm_intel nested=1";
  virtualisation.libvirtd.enable = true;
  users.users.b.extraGroups = [ "libvirtd" ];
}
