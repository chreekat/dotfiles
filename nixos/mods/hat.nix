{ pkgs, ... }:

{
  # Patch the vendored libvterm to carry the SGR 2 (faint/dim) cell attribute
  # it otherwise drops, so hat renders dim text (bug 33/48). hat is the
  # canonical home for the patch; we reference it from the live checkout, the
  # same way the package itself is built from disk below.
  nixpkgs.overlays = [
    (final: prev: {
      libvterm-neovim = prev.libvterm-neovim.overrideAttrs (o: {
        patches = (o.patches or [ ])
          ++ [ /home/b/Projects/hat/nix/libvterm-dim.patch ];
      });
    })
  ];

  # hat, the terminal multiplexer (https://git.sr.ht/~chreekat/hat) —
  # packaged from the local checkout, so a rebuild deploys whatever is
  # on disk there.
  environment.systemPackages =
    [ (pkgs.callPackage /home/b/Projects/hat/package.nix { }) ];
}
