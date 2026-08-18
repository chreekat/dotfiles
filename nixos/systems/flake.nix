{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-puny.url = "github:NixOS/nixpkgs/nixos-26.05";
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
    #
    # HE SO PUNY
    #
    nixosConfigurations.puny = nixpkgs-puny.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ../mods/igtest.nix
        ../mods/irc-bouncer.nix
        ../mods/nix-hygiene.nix
        ../mods/ntfy.nix
        ../mods/server-ssh.nix
        ../mods/server-www-fileserv.nix
        ../mods/syncthing.nix
        ../mods/tailscale.nix
        ../mods/user-b.nix
        ./puny/configuration.nix
        ./server-sudo.nix
      ];
    };
    nixosConfigurations.honk = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        disko.nixosModules.disko
        agenix.nixosModules.default
        ../mods/matrix-server.nix
        ../mods/nix-hygiene.nix
        ../mods/server-ssh.nix
        ../mods/snowdrift-gitlab.nix
        ../mods/user-b.nix
        ./honk/configuration.nix
        ({ pkgs, ... }: { environment.systemPackages = [ pkgs.borgbackup ]; })
      ];
    };
  };
}
