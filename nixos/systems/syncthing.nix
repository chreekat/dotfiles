{
  services.syncthing =
    let
      devices = builtins.fromJSON (builtins.readFile ./syncthing-devices.json);
      dataDir = "/home/b/Syncthing";
      addPath = name: val: val // { path = "${dataDir}/${name}"; };
    in {
      inherit devices dataDir;
      enable = true;
      user = "b";
      group = "b";
      configDir = dataDir + "/.config/syncthing";
      folders = builtins.mapAttrs addPath {
        PhoneFiles = {
          id = "0acam-5wio6";
          devices = [ "Pixel 6a" "fuzzbomb" "smilga" "kuusi" ];
        };
        Brarris = {
          id = "pgfad-5ilgg";
          devices = [ "kuusi" "tallac" ];
        };
      };
    };
}
