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
        "17:15"
      ];
    };
    settings = {
      # Needed for various good things
      trusted-users = ["b"];
      experimental-features = [ "nix-command" "flakes" ];
      # "allow-substitutes = false is an antifeature". It's almost never what
      # you want, and yet e.g. the top-level nixos system builder sets it. This
      # causes rebuilds of dependencies for some reason I don't quite
      # understand. But I've seen this happen with seemingly random rebuilds on
      # gitlab.haskell.org, so I believe it.
      always-allow-substitutes = true;
    };
  };
}
