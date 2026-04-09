{ pkgs ? import <nixpkgs> {} }:
let
  hpkgs = pkgs.haskellPackages;
  drv = hpkgs.callPackage ./default.nix {};
in
hpkgs.shellFor {
  packages = _: [ drv ];
  nativeBuildInputs = [
    hpkgs.cabal-install
    hpkgs.ghcid
  ];
}
