# Self-hosted ntfy push notification server.
# Tailscale-only access via MagicDNS (e.g., http://puny:8080).
{ ... }:
{
  services.ntfy-sh = {
    enable = true;
    settings = {
      listen-http = ":8080";
      base-url = "http://puny:8080";
    };
  };
}
