{ config, pkgs, lib, ... }:
{
  services.tlp.enable = true;
  # Keep charge less than max when plugged in. Saves battery life.
  services.tlp.settings = {
    START_CHARGE_THRESH_BAT0 = 90;
    STOP_CHARGE_THRESH_BAT0 = 92;
  };

  # Prevent disaster; be useful
  services.logind.powerKey = "suspend";

  services.autorandr.enable = true;
  services.autorandr.matchEdid = true;
  # Allow autorandr to retry more aggressively when displays take a moment to settle.
  systemd.services.autorandr.startLimitBurst = lib.mkForce 5;
  systemd.services.autorandr.serviceConfig.ExecStart =
    let cfg = config.services.autorandr;
    in
      lib.mkForce ''
        ${pkgs.autorandr}/bin/autorandr \
        --batch \
        --change \
        --default ${cfg.defaultTarget} \
        ${lib.optionalString cfg.ignoreLid "--ignore-lid"} \
        ${lib.optionalString cfg.matchEdid "--match-edid"} ;
        sleep 1
      '';
}
