{ config, ... }:

{
  # Used for developing cosmos. Services running in docker (traefik, authengine)
  # need to access services running on the host. The firewall prevents this by
  # default.
  networking.firewall.interfaces.cosmos-local.allowedTCPPorts = [ 8088 ];

  # Read access to the CentralApp Nix cache. The token lives in the
  # encrypted netrc, deployed outside the store — never inline it here,
  # this repo is public. Setup/rotation: cosmos docs/local-development.md
  # -> "Reading the Nix cache".
  age.secrets.nix-cache-netrc.file = ../agenix-secrets/nix-cache-netrc.age;
  nix.settings = {
    extra-substituters = [ "https://dors6r8kw7csx.cloudfront.net" ];
    extra-trusted-public-keys = [ "centralapp-nix-cache:Jp+Gc/C2muXT+/R0nYd7FlzxD5Jgfhixb0Qm4RLg+fs=" ];
    netrc-file = config.age.secrets.nix-cache-netrc.path;
  };
}
