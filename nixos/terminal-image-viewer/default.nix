{ stdenv, lib, fetchFromGitHub, pkgconfig, imagemagick }:

stdenv.mkDerivation {
  name = "tiv-1.0-f78b65c1f375";

  src = fetchFromGitHub {
    owner = "stefanhaustein";
    repo = "TerminalImageViewer";
    rev = "f78b65c1f375e688c7500f644a1766e48b783933";
    sha256 = "055qf1fpapvrckkwlx1zsy1z4dyr87zjj79l7z6vh7d54fs4alpv";
  };

  buildInputs = [ imagemagick ];

  sourceRoot = "source/src/main/cpp";

  makeFlags = [ "prefix=$(out)" ];

  meta = {
    description = "Display images in a modern terminal";
    homepage = https://github.com/stefanhaustein/TerminalImageViewer;
    license = lib.licenses.asl20;
    maintainers = [ lib.maintainers.chreekat ];
    platforms = lib.platforms.all;
  };
}
