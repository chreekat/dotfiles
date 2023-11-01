{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  };

  outputs = { self, nixpkgs }:  {
    nixosConfigurations.puny = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./puny/configuration.nix
      ];
    };
  };
}
