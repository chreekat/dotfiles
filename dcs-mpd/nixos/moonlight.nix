# The Thinkpad side of dcs-mpd: Moonlight, and `dcs-mpd` to show smilga's
# virtual monitor fullscreen. Pair once by hand with `moonlight pair smilga`.
{ pkgs, ... }:
let
  dcs-mpd = pkgs.writeShellScriptBin "dcs-mpd" ''
    # Compiled QML is rebuilt on every start rather than read from
    # ~/.cache, which outlives a moonlight-qt upgrade.
    export QML_DISABLE_DISK_CACHE=1
    exec ${pkgs.moonlight-qt}/bin/moonlight stream smilga Desktop \
      --display-mode fullscreen \
      --resolution 1920x1080 \
      --fps 60 \
      --bitrate 20000 \
      --audio-on-host \
      --quit-after \
      "$@"
  '';
in
{
  environment.systemPackages = [
    pkgs.moonlight-qt
    dcs-mpd
  ];
}
