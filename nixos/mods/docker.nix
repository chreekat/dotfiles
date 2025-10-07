{ pkgs, ... }:
{
  # Fixes a bug in iptables,
  # https://github.com/NixOS/nixpkgs/issues/417641#issuecomment-3280834393
  # TODO: Well, eventually. I don't have the diskspace for the rebuild this requires.
  #nixpkgs.overlays = [
  #  (final: prev:
  #    {
  #      iptables = prev.iptables.overrideAttrs (old:
  #        # Guard against upstream updates so we remember to drop the patch once 1.8.11 is no longer needed.
  #        assert old.version == "1.8.11";
  #        {
  #          patches = old.patches or [] ++ [ ../patches/iptables-broke-docker.patch ];
  #        });
  #    }
  #  )
  #];

  # Instead, brute force accept in br+ interface.
  networking.firewall = assert pkgs.iptables.version == "1.8.11"; {
    extraCommands = "
      iptables -I nixos-fw 1 -i br+ -j ACCEPT
    ";
    extraStopCommands = "
      iptables -D nixos-fw -i br+ -j ACCEPT
    ";
  };

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };
  environment.systemPackages = [
    pkgs.docker-credential-helpers
  ];
}
