{ config, pkgs, ... }:
{
  nix.settings.trusted-users = [ "b" ];

  users.extraGroups.b = {
    gid = 1000;
  };
  users.users.b = {
    isNormalUser = true;
    uid = 1000;
    group = "b";
    extraGroups = ["users" "wheel" "video" "systemd-journal"];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICzNy6bOnkcu54nPeN523uvfRq3WbGCgEbTQifWLF+D0 b@kuusi"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC5wfyVQYW4sBkZ7a9dNlrGorUXQc1BvHmhxBmmrrwAKs/kkvaS1jgraDvsjo+xo4skiI9HwVXP7OMhjjdLH6MMk3X0Sewv+V8V6xDmpr6NYZKgmFx9b0tPp4IBRsuZu5vbNDyIQo/DbNFzCKT2+ax5DKy/GBahH1WMq4Ks+N01sBrXlOl40fnihdcYTpEZ5jJYheAjrgKpcmoAV364C4lNFfUGxkYVM0DAs39yIahDWQ7bReuzaJRhz+CpfAfNLTpIU1vnWP0yv4ZSRM47eQ59dYVieLqZFxroJ1xXfkdSR10AjCdcmOZWa9Bx9qBF5I5kCdPbW559D6QaROsy4EUzqGOuY8ynALm0zJDXrEj5rTxV8vJfHjQ+m8+oXUdRxdJtbrf0al8mytD80hl50mdzlhomWFiEJO9ny2N4AZiJJ0xbIhAFvHAjaTwTa+j6uF0OFIobcqrtaGjjBYXG8FPdJCNByt7N7g8UOTggfzQoxf3g3dBx79gfBbuKFEohTI7XvgEAvFFVrW3NHn7eYN4QKfqO51ns6atmJDIo0Phpjnz0670I+Od9WETa/IJcNjJkcA22612hhRtNK5NZcgKTRijNStegFAtY7ZApB2nMj5lQcRA3Dps6lF1PpICIj5IBF9R4n5AeR405l9QG113iqvAIezHgYHSltOgHBmGOMw== b@fuzzbomb"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICXYHLQLP5E9vLzXPaht5rM6T5V7VtFyb48Ep2VCI2Nn bryan@omena"
    ];
  };

  security.sudo.execWheelOnly = true;
  security.sudo.wheelNeedsPassword = false;
}
