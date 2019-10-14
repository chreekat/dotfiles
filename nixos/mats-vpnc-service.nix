{ lib, pkgs, config, ... }:

with lib;

let

  localGatewaySave = "/var/run/vpnc/LOCAL_GATEWAY";
  buildVpnc = key: value: ''
    IPSec gateway ${value.gateway}
    IPSec ID ${value.id}
    IPSec secret ${value.secret}
    Xauth username ${value.username}
    Password helper ${pkgs.systemd}/bin/systemd-ask-password
    Script ${pkgs.vpnc}/etc/vpnc/vpnc-script
  '';
  up = pkgs.writeScript "up" ''
    #!${pkgs.bash}/bin/bash
    PATH=${makeBinPath [pkgs.nettools pkgs.iproute]}:$PATH

    LOCAL_GATEWAY=$(ip r | grep "default via" | grep -oE "([0-9]+\.[0-9]+\.[0-9]+\.[0-9]+)" | head -n1)
    ip route del default via $LOCAL_GATEWAY
    ip route add default dev tun0 proto static scope link metric 50
    if [ ! -d /var/run/vpnc ]; then
      mkdir -p /var/run/vpnc
    fi
    # This file also serves as a flag, used in 'down'
    echo -ne $LOCAL_GATEWAY > ${localGatewaySave}
  '';
  down = pkgs.writeScript "down" ''
    #!${pkgs.bash}/bin/bash
    PATH=${makeBinPath [pkgs.nettools pkgs.iproute]}:$PATH

    if [ -e ${localGatewaySave} ]; then
      LOCAL_GATEWAY=$(cat ${localGatewaySave})
      ip route add default via $LOCAL_GATEWAY
      ip route del default dev tun0
      rm ${localGatewaySave}
    fi
  '';
  buildService = key: value: nameValuePair "vpnc-${key}" {
    description = "VPNC for ${key}";
    # No automatic startup, password

    path = [ pkgs.iptables pkgs.nettools pkgs.iproute ];
    serviceConfig = {
      ExecStart = "${pkgs.vpnc}/bin/vpnc /etc/vpnc/${key}.conf";
      ExecStartPost = "${up}";
      ExecStopPost = "${down}";
      Type = "forking";
    };
  };

  cfg = config.services.vpnc;
  vpnModule = types.submodule {
    options = {
      gateway = mkOption {
        default = "";
        type = types.str;
        description = "Gateway address";
      };
      id = mkOption {
        default = "linux";
        type = types.str;
        description = "Your group name";
      };
      secret = mkOption {
        default = "";
        type = types.str;
        description = "Your group secret";
      };
      username = mkOption {
        default = "";
        type = types.str;
        description = "Your username";
        example = "john.doe@example.com";
      };
    };
  };

in

{
  options.services.vpnc.servers = mkOption {
    default = {};

    description = "Each attribute defines a vpn client";

    type = types.attrsOf vpnModule;
  };

  config = mkIf (cfg.servers != {}) {
    environment.systemPackages = [ pkgs.vpnc ];
    boot.kernelModules = [ "tun" ];
    networking.vpnc.services = mapAttrs buildVpnc cfg.servers;
    systemd.services = listToAttrs (mapAttrsFlatten buildService cfg.servers);
  };
}
