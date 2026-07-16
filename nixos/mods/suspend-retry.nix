{ pkgs, ... }:
{
  # Suspend can abort outright when amdgpu fails to evict VRAM under memory
  # pressure (-ENOMEM in suspend prepare), leaving the machine fully awake
  # with the lid closed. Free memory and retry instead of staying awake.
  # The start rate limit bounds the retry loop if suspend keeps failing.
  systemd.services.suspend-retry = {
    description = "Free memory and retry a failed suspend";
    startLimitIntervalSec = 600;
    startLimitBurst = 3;
    serviceConfig.Type = "oneshot";
    script = ''
      ${pkgs.coreutils}/bin/sync
      echo 3 > /proc/sys/vm/drop_caches
      echo 1 > /proc/sys/vm/compact_memory
      ${pkgs.coreutils}/bin/sleep 2
      ${pkgs.systemd}/bin/systemctl suspend
    '';
  };
  systemd.services.systemd-suspend.onFailure = [ "suspend-retry.service" ];
}
