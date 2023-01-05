{ config, pkgs, ... }:
{
  security.sudo.execWheelOnly = true;
  security.sudo.wheelNeedsPassword = false;
}
