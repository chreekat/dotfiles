# The Thinkpad side of dcs-mfd: Moonlight, and `dcs-mfd` to show smilga's
# virtual monitor fullscreen. Pair once by hand with `moonlight pair smilga`.
{ pkgs, ... }:
let
  dcs-mfd = pkgs.writeShellScriptBin "dcs-mfd" ''
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
    dcs-mfd
  ];
}
