{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, disko }:  {
    nixosConfigurations.puny = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./puny/configuration.nix
      ];
    };
    nixosConfigurations.honk = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        disko.nixosModules.disko
        ./honk/configuration.nix
      ];
    };
  };
}
