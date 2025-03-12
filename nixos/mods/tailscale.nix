{ config, ... }:
{
  services.tailscale.enable = true;
  services.tailscale.extraUpFlags = [ "--ssh" ];
  networking.firewall.trustedInterfaces = [ config.services.tailscale.interfaceName ];

  networking.hosts = {
    # Glowing Bear and InvoicePlane running on puny.
    "100.81.115.78" = [ "glowing-bear.chreekat" "invoices.chreekat" ];
  };
}
