# The Thinkpad side of dcs-mpd: Moonlight, and `dcs-mpd` to show smilga's
# virtual monitor fullscreen. Pair once by hand with `moonlight pair smilga`.
{ pkgs, ... }:
let
  dcs-mpd = pkgs.writeShellScriptBin "dcs-mpd" ''
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
