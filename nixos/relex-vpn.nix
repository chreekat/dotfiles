{config, lib, pkgs, ...}:

let

  vpn = ''
      authby=secret
      pfs=no
      auto=add
      rekey=no
      type=transport
      left=%defaultroute
      right=84.20.147.37
      rightprotoport=17/1701
      ike=3des-sha1;modp1024
      phase2alg=3des-sha1
    '';

  # From libreswan.nix
  # FIXME: Fix the libreswan package and just use it directly.
  indent = with pkgs.lib; str: concatStrings (concatMap (s: ["  " (trim [" " "\t"] s) "\n"]) (splitString "\n" str));
  trim = with pkgs.lib; chars: str: let
      nonchars = filter (x : !(elem x.value chars))
                  (imap0 (i: v: {ind = i; value = v;}) (stringToCharacters str));
      in
        if length nonchars == 0 then ""
        else substring (head nonchars).ind (add 1 (sub (last nonchars).ind (head nonchars).ind)) str;


  start = ''
    #!/usr/bin/env bash

    systemctl restart ipsec xl2tpd

    echo "Waiting for ipsec and xl2tpd to restart..."
    sleep 5

    LOCAL_GATEWAY=$(ip r | grep "default via" | grep -oE "([0-9]+.[0-9]+.[0-9]+.[0-9]+)" | head -n1)
    REMOTE_ENDPOINT=192.168.168.168

    ipsec auto --up relexvpn

    echo "c relexvpn" | sudo tee /var/run/xl2tpd/control

    echo "Waiting for xl2tpd control command..."
    sleep 5

    ip route add 84.20.147.37 via $LOCAL_GATEWAY

    # The things we actually want from the VPN
    # Wiki
    ip route add 10.201.185.89 via $REMOTE_ENDPOINT
    # Gitlab
    ip route add 10.201.7.240 via $REMOTE_ENDPOINT
    # gw1.relex.fi
    ip route add 83.150.98.71 via $REMOTE_ENDPOINT
    # ???
  '';

  stop = ''
    #! /usr/bin/env bash
    systemctl stop xl2tpd ipsec
    echo "Waiting for xl2tpd and ipsec services to stop..."
    sleep 5
    systemctl restart wpa_supplicant
  '';

  pppdOptions = pkgs.writeText "pppd.options" ''
    ipcp-accept-local
    ipcp-accept-remote
    refuse-eap
    require-mschap-v2
    noccp
    noauth
    idle 1800
    defaultroute
    usepeerdns
    debug
    connect-delay 5000
    name ${config.relex.vpn.username}
    file /root/relex-vpn.pppd-secrets
  '';

in

{
  options = {
    relex.vpn.username = lib.mkOption {
      type = lib.types.str;
      description = ''
        Your IPA username. Remember to add your password to
        /root/relex-vpn.pppd-secrets:

          password "<password>"
      '';
    };
  };

  config = {
    # FIXME secretsfile declared twice?

    environment.etc = {
      "ipsec.d/connections.conf" = {
        text = ''
          conn relexvpn
          ${indent vpn}
        '';
      };
      "ipsec.conf" = {
        text = ''
          config setup
            secretsfile=/root/relex-vpn.ipsec-secrets
          include /etc/ipsec.d/connections.conf
        '';
      };
    };

    services.libreswan = {
      enable = true;
      configSetup = ''
        secretsfile=/root/relex-vpn.ipsec-secrets
      '';
      connections."relexvpn" = "${vpn}";
    };
    environment.systemPackages = with pkgs; [
      (writeScriptBin "vpn-start" "${start}")
      (writeScriptBin "vpn-stop" "${stop}")
    ];

    boot = {
      kernelModules = [
        "af_key"
        "l2tp_ppp"
      ];
    };

    services.xl2tpd = {
      enable = true;
      extraXl2tpOptions = ''

        [lac relexvpn]
        lns = 84.20.147.37
        ppp debug = yes
        pppoptfile = ${pppdOptions}
        length bit = yes
        bps = 100000
      '';
    };
  };
}
