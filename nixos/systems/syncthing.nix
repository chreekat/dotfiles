{
  services.syncthing =
    let
      devices = {
        "Pixel 6a" = {
          id = "";
        };
        fuzzbomb = {
          id = "";
        };
        smilga = {
          id = "";
        };
        tallac = {
          id = "";
        };
        kuusi = {
          id = "";
        };
      };
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
          id = "";
          devices = builtins.attrNames devices;
        };
        Brarris = {
          id = "";
          devices = [ "kuusi" "tallac" ];
        };
      };
    };
}
