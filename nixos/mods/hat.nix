{ pkgs, ... }:

{
  # ghc-debug-brick's attribute map is hardcoded -- no theme file, no flag --
  # and its default attribute carries a blanket dim, so every unnamed piece of
  # text renders faint in a terminal that honours SGR 2. hat does.
  nixpkgs.overlays = [
    (final: prev: {
      haskellPackages = prev.haskellPackages.extend (_: hprev: {
        ghc-debug-brick =
          prev.haskell.lib.appendPatch hprev.ghc-debug-brick
            ../patches/ghc-debug-brick-contrast.patch;
      });
    })
  ];

  # hat, the terminal multiplexer (https://git.sr.ht/~chreekat/hat) —
  # packaged from the local checkout, so a rebuild deploys whatever is
  # on disk there.
  environment.systemPackages = [
    (pkgs.callPackage /home/b/Projects/hat/package.nix { ghcDebug = true; })
    pkgs.haskellPackages.ghc-debug-brick
  ];
}
