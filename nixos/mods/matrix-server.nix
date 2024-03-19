{ pkgs, config, lib, ... }:
let
  fqdn = "synapse.chreekat.net";
  serverName = "chreekat.net";
  baseUrl = "https://${fqdn}";
in {
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  # User (needed when matrix-synapse.enable = false)

  users.users.matrix-synapse = lib.mkDefault {
    isSystemUser = true;
    home = "/var/lib/matrix-synapse";
    group = "matrix-synapse";
    createHome = true;
    description = "Synapse user";
  };
  users.groups.matrix-synapse = {};

  # Secrets

  age.secrets.synapse-secrets-config = {
    file = ../agenix-secrets/synapse-secrets-config.yaml.age;
    owner = "matrix-synapse";
    group = "matrix-synapse";
    mode = "440";
  };
  age.secrets.synapse-signing-key = {
    file = ../agenix-secrets/synapse-signing-key.age;
    owner = "matrix-synapse";
    group = "matrix-synapse";
    mode = "440";
  };

  # Postgresql

  services.postgresql.enable = true;
  # synapse requires nondefault LC_COLLATE and LC_CTYPE.
  services.postgresql.initialScript = pkgs.writeText "synapse-init.sql" ''
    CREATE DATABASE "matrix-synapse"
      TEMPLATE template0
      LC_COLLATE = "C"
      LC_CTYPE = "C";
  '';
  # This just makes ensureUsers happy since we create the db ourselves already.
  services.postgresql.ensureDatabases = [ "matrix-synapse" ];
  services.postgresql.ensureUsers = [
    {
      name = "matrix-synapse";
      ensureDBOwnership = true;
    }
  ];

  # Nginx

  services.nginx = {
    enable = true;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;
    virtualHosts = {
      "${fqdn}" = {
        enableACME = true;
        forceSSL = true;
        locations."/".proxyPass = "http://localhost:8008";
      };
    };
  };

  # Synapse

  services.matrix-synapse = {
    enable = true;
    extraConfigFiles =  [
      config.age.secrets.synapse-secrets-config.path
    ];
    settings.server_name = serverName;
    settings.public_baseurl = baseUrl;
    settings.signing_key_path = config.age.secrets.synapse-signing-key.path;
    # TODO enable sliding sync?
  };
}
