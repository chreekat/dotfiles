{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/vda";
  };

  networking = {
    hostName = "general-1";
    useDHCP = false;
    interfaces.ens3.useDHCP = true;
  };

  services = {
    openssh = {
      enable = true;
      permitRootLogin = "no";
    };
  };

  system.stateVersion = "19.09";

  time.timeZone = "Europe/Amsterdam";

  users.users.b = {
    extraGroups = ["wheel"];
    isNormalUser = true;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDGtjJgc1ltodKGR7ZkF60JM/0l3NjpWZISZMhn8JXMOqbesbKH/AEP52HQ4vZiLQdfGIUbzNwJnPoHIWMLogikVFvAswl2cGmj2yzWmN+JadFa/gFeDbAZOIahecHf3jmIIwutADDiCExtp8ztglFMr8nVruDa0xfQtQ0dZcAgJxduTJPOjuwt5QYipH7m6Af1M0HgDPdr+pgRZubnEi5Sa8w5Bv6WxvZ/fT3ULEQLPJGP+4tqV1VV3r6lQThgMvRNOR1EL5Wt8UFTvOODHxTYBJ+qbwc+E0FrcPHK181Q3swle7Q8a3ZjuT5yHBwps/ikE3nEyPXqln3QztIBDVpeU1qsqyH6jhDMI82GJkXU+QOJ57lGzIo1FPLmGla77jfRFV/fsdy/LBNjyi8bfDhE+orPupkQW4+EjJS3+FVBlxZzL0kJufjG5i5X/KZkmFIk4RhvizAPGgABM2byiDz0ZDGFnyN1WHsBMo4dspfhC2X9eiziTUsf/wm3ltigosxO/Dhx03CbGtmQZNaQ2/ePoKJ9P1HjTOOYTObp0ZmaTMydEHZ/En3efTF5nfY2nplyblkpYCWjWpkn7sgG1sCXZIMZHsVJhDZyrN7XqURXmRqBeAADjN5xzZoSMUt2d6j4QHAQYvVXJxPuqbZXjhCdnxpcgioOe6go7nWtNMWQoQ== b@bryan-laptop"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC5wfyVQYW4sBkZ7a9dNlrGorUXQc1BvHmhxBmmrrwAKs/kkvaS1jgraDvsjo+xo4skiI9HwVXP7OMhjjdLH6MMk3X0Sewv+V8V6xDmpr6NYZKgmFx9b0tPp4IBRsuZu5vbNDyIQo/DbNFzCKT2+ax5DKy/GBahH1WMq4Ks+N01sBrXlOl40fnihdcYTpEZ5jJYheAjrgKpcmoAV364C4lNFfUGxkYVM0DAs39yIahDWQ7bReuzaJRhz+CpfAfNLTpIU1vnWP0yv4ZSRM47eQ59dYVieLqZFxroJ1xXfkdSR10AjCdcmOZWa9Bx9qBF5I5kCdPbW559D6QaROsy4EUzqGOuY8ynALm0zJDXrEj5rTxV8vJfHjQ+m8+oXUdRxdJtbrf0al8mytD80hl50mdzlhomWFiEJO9ny2N4AZiJJ0xbIhAFvHAjaTwTa+j6uF0OFIobcqrtaGjjBYXG8FPdJCNByt7N7g8UOTggfzQoxf3g3dBx79gfBbuKFEohTI7XvgEAvFFVrW3NHn7eYN4QKfqO51ns6atmJDIo0Phpjnz0670I+Od9WETa/IJcNjJkcA22612hhRtNK5NZcgKTRijNStegFAtY7ZApB2nMj5lQcRA3Dps6lF1PpICIj5IBF9R4n5AeR405l9QG113iqvAIezHgYHSltOgHBmGOMw== b@fuzzbomb"
    ];
  };
}
