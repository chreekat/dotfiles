{ pkgs, ... }:
let tailscaleIP = "100.81.115.78";
in
{
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
