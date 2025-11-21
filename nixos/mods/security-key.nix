{ pkgs, ... }:
{
  environment.systemPackages = [
    pkgs.nitrokey-app2
    pkgs.nitrocli
    pkgs.yubioath-flutter
    pkgs.yubikey-manager
    pkgs.pam_u2f
  ];
  hardware.nitrokey.enable = true;
  users.users.b.extraGroups = [ "nitrokey" ];
  security.pam.u2f = {
    enable = true;
    settings = {
      cue = true;
      pinverification = 1;
    };
  };
}
