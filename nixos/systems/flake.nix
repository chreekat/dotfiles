{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    # Stuck on 24.11 because weechat was broken
    nixpkgs-puny.url = "github:NixOS/nixpkgs/nixos-24.11";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixpkgs-puny, disko, agenix }:  {
    nixosConfigurations.kuusi = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ ../configuration-kuusi.nix ];
    };
    nixosConfigurations.puny = nixpkgs-puny.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ../upstream-patches/invoiceplane.nix
        ./puny/configuration.nix
        ./user-b.nix
        ./server-sudo.nix
        ../mods/server-ssh.nix
        ../mods/tailscale.nix
        ../mods/server-www-fileserv.nix
        ../mods/syncthing.nix
        ../mods/nix-hygiene.nix
        ../mods/irc-bouncer.nix
        ../mods/server-invoiceplane.nix
      ];
    };
    nixosConfigurations.honk = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        disko.nixosModules.disko
        agenix.nixosModules.default
        ./honk/configuration.nix
        ./user-b.nix
        ../mods/snowdrift-gitlab.nix
        ../mods/nix-hygiene.nix
        ../mods/server-ssh.nix
        ../mods/matrix-server.nix
        ({ pkgs, ... }: { environment.systemPackages = [ pkgs.borgbackup ]; })
      ];
    };
  };
}
