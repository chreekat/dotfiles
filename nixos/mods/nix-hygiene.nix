{
  # Default is 100. Wtf, NixOS?
  boot.loader.grub.configurationLimit = 20;
  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 14d";
    };
    optimise = {
      automatic = true;
      dates = [
        "03:45"
        "17:15"
      ];
    settings = {
      # Needed for various good things
      trusted-users = ["b"];
      experimental-features = [ "nix-command" "flakes" ];
    };
  };
}
