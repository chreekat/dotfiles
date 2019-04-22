{ pkgs ? import <nixpkgs> {} }:
with pkgs;
stdenv.mkDerivation {
  name = "b-dotfiles";
  src = ./.;
  phases = ["unpackPhase" "buildPhase" "installPhase"];
  buildPhase = ''
    substituteInPlace deploy.sh --subst-var-by EXPORT_DIR ''$out/lib
  '';
  installPhase = ''
    mkdir -p ''$out/{bin,lib}
    chmod 755 deploy.sh
    mv deploy.sh ''$out/bin/deploy-dotfiles
    cp -r * ''$out/lib
  '';
}
