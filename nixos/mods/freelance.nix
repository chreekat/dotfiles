{ pkgs, ... }:
{
  environment.systemPackages = [ (pkgs.callPackage ../packages/invoicer {}) ];
}
