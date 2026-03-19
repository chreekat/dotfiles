{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    element-desktop
    signal-desktop
    slack
    # not cached?!?
    # telegram-desktop
    wasistlos # whatsapp
  ];
}
