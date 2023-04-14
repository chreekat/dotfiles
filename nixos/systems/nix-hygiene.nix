{
  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    settings = {
      # Needed for various good things
      trusted-users = ["b"];
      experimental-features = [ "nix-command" "flakes" ];
    };
  };
}
