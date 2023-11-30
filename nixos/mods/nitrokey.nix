{ pkgs, ... }:
{
  # broken :(
  environment.systemPackages = [ pkgs.pynitrokey ];
  hardware.nitrokey.enable = true;
  users.users.b.extraGroups = [ "nitrokey" ];
  #security.pam.u2f.enable = true;
}
