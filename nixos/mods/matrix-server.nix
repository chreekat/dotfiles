{ pkgs, config, ... }:
let
  fqdn = "synapse.chreekat.net";
  serverName = "chreekat.net";
  baseUrl = "https://${fqdn}";
in {
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  # User

  users.users.matrix-synapse = {
    isSystemUser = true;
    home = "/var/lib/user-matrix-synapse";
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
    enable = false;
    settings.server_name = serverName;
    settings.public_baseurl = baseUrl;
    settings.extraConfigFiles =  [
      config.age.secrets.synapse-secrets-config.path
    ];
    settings.signing_key_path = config.age.secrets.synapse-signing-key.path;
    # TODO enable sliding sync?
  };
}
