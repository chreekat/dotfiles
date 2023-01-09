{

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    cachix-deploy-flake.url = "github:cachix/cachix-deploy-flake";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, cachix-deploy-flake }:
    flake-utils.lib.eachDefaultSystem (
      system: {
        defaultPackage = let
          pkgs = import nixpkgs { inherit system; };
          cachix-deploy-lib = cachix-deploy-flake.lib pkgs;
        in
          cachix-deploy-lib.spec {
            agents = {
              myagent = cachix-deploy-lib.nixos ./configuration.nix;
            };
          };
      }
    );
}
