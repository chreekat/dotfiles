{ mkDerivation, base, bytestring, clock, containers, directory, evdev
, hinotify, lib, text
}:
mkDerivation {
  pname = "beyboard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring clock containers directory evdev hinotify text
  ];
  description = "Capslock-as-ctrl/escape at the evdev level";
  license = lib.licenses.bsd3;
  mainProgram = "beyboard";
}
