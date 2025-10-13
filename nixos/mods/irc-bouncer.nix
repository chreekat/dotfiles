{ config, pkgs, ... }:
let tailscaleIP = "100.81.115.78";
in
{
  # Fix bug introduced by
  # https://github.com/NixOS/nixpkgs/commit/e66ee6f45f1ce59a07a5fc88f2c95bb3dc06a01a
  users.users.weechat.createHome = true;
  users.users.weechat.home = config.services.weechat.root;

  services.weechat = {
    enable = true;
  };
  services.nginx.enable = true;
  services.nginx.virtualHosts."glowing-bear.chreekat" =  {
    forceSSL = false;
    root = "${pkgs.glowing-bear}";
    listenAddresses = [ "${tailscaleIP}" ];
  };
}
