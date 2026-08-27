# Packages I'm developing locally I want avalailable in the system.
{ pkgs, ... }:

{
  # ghc-debug-brick's attribute map is hardcoded -- no theme file, no flag --
  # and its default attribute carries a blanket dim, so every unnamed piece of
  # text renders faint in a terminal that honours SGR 2.
  nixpkgs.overlays = [
    (final: prev: {
      haskellPackages = prev.haskellPackages.extend (_: hprev: {
        ghc-debug-brick =
          prev.haskell.lib.appendPatch hprev.ghc-debug-brick
            ../patches/ghc-debug-brick-contrast.patch;
      });
    })
  ];

  environment.systemPackages = [
    (pkgs.callPackage /home/b/Projects/hat/package.nix { ghcDebug = true; })
    (pkgs.callPackage /home/b/Projects/4h/package.nix { })
    pkgs.haskellPackages.ghc-debug-brick
  ];
}
