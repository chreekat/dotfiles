{
  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-26.05/nixexprs.tar.zst";
    nixpkgs-kuusi.url = "https://channels.nixos.org/nixos-26.05/nixexprs.tar.zst";
    nixpkgs-puny.url = "https://channels.nixos.org/nixos-26.05/nixexprs.tar.zst";
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # The agenix CLI only, pinned apart from the module above so that bumping
    # the tool kuusi encrypts with does not touch honk's secret activation.
    agenix-cli = {
      url = "github:ryantm/agenix";
      flake = false;
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Submodule contents are invisible to flake evaluation, so the fonts come
    # in as an input rather than as ../overlays/nonfree-fonts.
    nonfree-fonts = {
      url = "git+ssh://git@git.sr.ht/~chreekat/nonfree-fonts";
      flake = false;
    };
    # 'b', the bug tracker.
    losh-t = {
      url = "github:sjl/t";
      flake = false;
    };
    # Locally developed, but pulled from origin rather than the checkouts in
    # ~/Projects so kuusi evaluates purely.
    hat.url = "github:chreekat/hat";
    "4h".url = "github:chreekat/4h";
    ghcid.url = "github:chreekat/ghcid";
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-kuusi, nixpkgs-puny, disko, agenix, ... }:  {
    nixosConfigurations.kuusi = nixpkgs-kuusi.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };
      modules = [
        ../mods/amdgpu.nix
        ../mods/beyboard.nix
        ../mods/centralapp.nix
        ../mods/desktop.nix
        ../mods/dynamic-derivations.nix
        ../mods/freelance.nix
        ../mods/local-packages.nix
        ../mods/laptop.nix
        ../mods/p4.nix
        ../mods/security-key.nix
        ../mods/server-ssh.nix
        ../mods/suspend-retry.nix
        ../mods/user-b.nix
        ../system-common.nix
        ./kuusi/configuration.nix
      ];
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
