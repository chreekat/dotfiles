{
  # Used for developing cosmos. Services running in docker (traefik, authengine)
  # need to access services running on the host. The firewall prevents this by
  # default.
  networking.firewall.interfaces.cosmos-local.allowedTCPPorts = [ 8088 ];
}
