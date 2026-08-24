{ config, pkgs, ... }:

let
  # FIXME make this a module!?
  tailscaleIP = "100.68.0.118";
  statefulness = {
    networking.hostName = "kuusi";
    system.stateVersion = "21.11";
    networking.hostId = "ce26c8ff";
  };
  # Synaptics Prometheus fingerprint reader occasionally STALLs its USB
  # endpoint, after which fprintd cannot init the device and PAM silently
  # drops to password-only at the lock screen. Recovery is a USB-level
  # unbind/bind; restarting fprintd afterwards clears its empty device list.
  fprint-reset = pkgs.writeShellScriptBin "fprint-reset" ''
    set -eu
    vid=06cb
    pid=00bd
    device=
    for dev in /sys/bus/usb/devices/*/; do
      if [ -r "$dev/idVendor" ] \
         && [ "$(cat "$dev/idVendor")" = "$vid" ] \
         && [ "$(cat "$dev/idProduct")" = "$pid" ]; then
        device=$(basename "$dev")
        break
      fi
    done
    if [ -z "$device" ]; then
      echo "fprint-reset: device $vid:$pid not found" >&2
      exit 1
    fi
    echo "$device" > /sys/bus/usb/drivers/usb/unbind
    echo "$device" > /sys/bus/usb/drivers/usb/bind
    ${pkgs.systemd}/bin/systemctl restart fprintd.service || true
  '';
in statefulness // {
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # (NOTE: Copied from fuzzbomb, values tweaked)
  # Balance cores and max-jobs experimentally.
  # Watching
  # https://discourse.nixos.org/t/are-there-concrete-suggestions-for-balancing-cores-and-max-jobs/11824
  # for new ideas.
  nix.settings.cores = 4;
  nix.settings.max-jobs = 4;
  # Twice the default, since I got a warning to increase it.
  nix.settings.download-buffer-size = 2 * 67108864;

  # A flake's nixpkgs source ships no programs.sqlite, unlike a channel
  # tarball, so command-not-found reads the root channel's copy.
  programs.command-not-found.dbPath =
    "/nix/var/nix/profiles/per-user/root/channels/nixos/programs.sqlite";

  services.openssh.enable = true;
  services.openssh.listenAddresses = [ { addr = tailscaleIP; port = 22; } ];

  services.logind.settings.Login.HandleLidSwitchExternalPower = "lock";

  environment.systemPackages = [ fprint-reset ];

  powerManagement.resumeCommands = ''
    ${fprint-reset}/bin/fprint-reset || true
  '';

  # Kuusi has a nvme disk; this should make it faster.
  boot.initrd.luks.devices.root.bypassWorkqueues = true;
}
