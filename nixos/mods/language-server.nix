{ pkgs, ...}: {
  environment.systemPackages = [
    pkgs.haskell-language-server
    pkgs.nodePackages.bash-language-server
    pkgs.python311Packages.python-lsp-server
    pkgs.nil
  ];
}
