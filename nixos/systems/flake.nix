{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
  };

  outputs = { self, nixpkgs, sops-nix }:  {
    nixosConfigurations.puny = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./puny/configuration.nix
      ];
    };
  };
}
