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
    wiki.url = "/home/b/HaskellFoundation/clones/haskell-wiki-configuration";
  };

  outputs = { self, nixpkgs, disko, agenix, wiki }:  {
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
        ({ pkgs, ... }: { environment.systemPackages = [ pkgs.borgbackup ]; })
        wiki.nixosModules.hawiki
        { services.hawiki = {
            enable = true;
            secure = true;
            url = "wikitest.chreekat.net";
            passFile = "/var/run/passwordlol";
          };
          services.nginx.virtualHosts."wikitest.chreekat.net" = {
            enableACME = true;
            forceSSL = true;
            locations."/" = {
              proxyPass = "http://localhost:8081";
              #extraConfig = ''
              #  proxy_set_header Host wikitest.chreekat.net;
              #'';
            };
          };
        }
      ];
    };
  };
}
