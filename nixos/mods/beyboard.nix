{ pkgs, ... }:
let
  beyboard = pkgs.haskellPackages.callPackage ./beyboard {};
in
{
  systemd.services.beyboard = {
    description = "Capslock as ctrl/escape remapper";
    wantedBy = [ "multi-user.target" ];
    after = [ "systemd-udevd.service" ];
    serviceConfig = {
      ExecStart = "${beyboard}/bin/beyboard";
      Restart = "always";
      RestartSec = 1;
    };
  };
}
