{ pkgs, ... }:

{
  # hat, the terminal multiplexer (https://git.sr.ht/~chreekat/hat) —
  # packaged from the local checkout, so a rebuild deploys whatever is
  # on disk there.
  environment.systemPackages =
    [ (pkgs.callPackage /home/b/Projects/hat/package.nix { }) ];
}
