{ pkgs, ...}:
let
  # Handy tool for tracking works in progress
  bugs =
    let
      losh-t = pkgs.python3Packages.buildPythonApplication {
        pname = "losh-t";
        version = "1.2.0";
        src = fetchGit {
          url = "https://github.com/sjl/t";
        };
      };
    in
    pkgs.writeScriptBin "b" ''
      set -Eeou pipefail
      topLevel=$(git rev-parse --git-common-dir)
      ${losh-t}/bin/t --task-dir $topLevel --list bugs $@
    '';
in {
  documentation.dev.enable = true;

  environment.systemPackages = with pkgs; [
    age
    (pkgs.callPackage "${builtins.fetchTarball "https://github.com/ryantm/agenix/archive/main.tar.gz"}/pkgs/agenix.nix" {})
    ssh-to-age
    bench
    bugs
    cachix
    code-cursor
    devenv
    dhall
    difftastic
    direnv
    emscripten
    entr
    git
    git-crypt
    nil
    niv
    nix-diff
    nix-prefetch
    nix-prefetch-docker
    nix-prefetch-github
    nix-prefetch-scripts
    nodejs
    shellcheck
    universal-ctags
    vim_configurable
  ] ++ [
    # Language servers (not Haskell, done separately)
    nodePackages.bash-language-server
    python3Packages.python-lsp-server
  ];

  programs.direnv.enable = true;
}
