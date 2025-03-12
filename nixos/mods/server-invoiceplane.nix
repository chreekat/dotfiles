{ config, ... }:
let tailscaleIP = "100.81.115.78";
in
{
  services.invoiceplane.webserver = "nginx";
  services.invoiceplane.sites."invoices.chreekat" = {
    enable = true;
    settings = {
      # "Set this setting to 'true' if you want to disable the setup for security purposes"
      # I ran it once, so let's disable now.
      DISABLE_SETUP = true;
    };
    webserver.listenAddresses = [ tailscaleIP ];
  };
}
