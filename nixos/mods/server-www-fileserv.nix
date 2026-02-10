{ config, lib, ... }:
let cfg = config.services.www-fileserv;
in
{
  options.services.www-fileserv = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        If `true`, enables a simple www fileserv with root at
        /var/www/''${networking.hostName} served at https://''${networking.fqdn}.
      '';
    };
    admin-email = lib.mkOption {
      type = lib.types.str;
      description = ''
        Email to use for ACME registration.
      '';
    };
  };

  config = {
    # Required to use ACME
    security.acme.defaults.email = cfg.admin-email;

    networking.firewall.allowedTCPPorts = [
      80
      443
    ];
    services.nginx.enable = lib.mkIf cfg.enable true;
    services.nginx.virtualHosts.${config.networking.fqdn} = lib.mkIf cfg.enable {
      forceSSL = true;
      enableACME = true;
      root = "/var/www/" + config.networking.hostName;
      # Make .html optional by rewriting
      extraConfig = ''
        location / {
          try_files $uri $uri.html =404;
        }
      '';
    };
  };
}
