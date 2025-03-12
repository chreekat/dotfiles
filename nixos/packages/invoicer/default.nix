{ pkgs, ... }:

pkgs.writeShellApplication {
  name = "invoicer";
  text = builtins.readFile ./invoicer.sh;
  runtimeInputs = [ pkgs.hledger ];
  # FIXME not implemented on my version of nixpkgs yet.
  # inheritPath = false;
}
