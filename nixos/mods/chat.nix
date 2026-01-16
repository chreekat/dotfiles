{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    element-desktop
    signal-desktop
    slack
    telegram-desktop
    wasistlos # whatsapp
  ];
}
