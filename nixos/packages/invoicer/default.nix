{ pkgs, ... }:

let
  python = pkgs.python3.withPackages (ps: [
    ps.google-api-python-client
    ps.google-auth-oauthlib
    ps.google-auth-httplib2
    ps.keyring
    ps.secretstorage
  ]);

  invoice-create = pkgs.writeScriptBin "invoice-create" ''
    #!${python}/bin/python3
    ${builtins.readFile ./invoice-create.py}
  '';

  invoicer = pkgs.writeShellApplication {
    name = "invoicer";
    text = builtins.readFile ./invoicer.sh;
    runtimeInputs = [
      pkgs.hledger
      invoice-create
    ];
  };

in
invoicer
