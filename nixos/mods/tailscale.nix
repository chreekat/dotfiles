{ config, ... }:
{
  services.tailscale.enable = true;
  services.tailscale.extraUpFlags = [ "--ssh" ];
  networking.firewall.trustedInterfaces = [ config.services.tailscale.interfaceName ];
}
