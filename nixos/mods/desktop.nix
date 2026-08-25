# See also xserver.nix
{ pkgs, ... }:
{
  security.pam.services.b.enableGnomeKeyring = true;
  services.gnome.gnome-keyring.enable = true;
  programs.seahorse.enable = true;

  # A systemd user unit doesn't inherit /etc/set-environment, so gcr's
  # ssh-agent needs SSH_ASKPASS handed to it directly.
  systemd.user.services.gcr-ssh-agent = {
    overrideStrategy = "asDropin";
    environment.SSH_ASKPASS = "${pkgs.seahorse}/libexec/seahorse/ssh-askpass";
  };

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common.default = "gtk";
  };
}
