{ pkgs, ... }:
{
  # broken :(
  # environment.systemPackages = [ pkgs.pynitrokey ];
  security.pam.u2f.enable = true;
}
