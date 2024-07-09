{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, disko, agenix }:  {
    nixosConfigurations.kuusi = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ ../configuration-kuusi.nix ];
    };
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
        agenix.nixosModules.default
        ./honk/configuration.nix
        ./user-b.nix
        ../mods/nix-hygiene.nix
        ../mods/server-ssh.nix
        ../mods/matrix-server.nix
      ];
    };
  };
}
