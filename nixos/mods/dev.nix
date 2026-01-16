{ pkgs, ...}:
let
  # Handy tool for tracking works in progress
  bugs =
    let
      losh-t = pkgs.python3Packages.buildPythonApplication {
        pname = "losh-t";
        format = "setuptools";
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
    act
    age
    (pkgs.callPackage "${builtins.fetchTarball "https://github.com/ryantm/agenix/archive/main.tar.gz"}/pkgs/agenix.nix" {})
    ssh-to-age
    bench
    bugs
    beekeeper-studio
    cachix
    #devenv # broken
    dhall
    difftastic
    amazon-ecr-credential-helper
    emscripten
    entr
    gh
    git
    git-crypt
    just
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
    urlencode
    vim-full
    zgrviewer # graphviz, dot
  ] ++ [
    # Language servers (not Haskell, done separately)
    nodePackages.bash-language-server
    python3Packages.python-lsp-server
  ];

  programs.direnv.enable = true;

  programs.nix-ld.enable = true;

  # Necessary in 25.05, but should be unnecessary in 25.11
  # Nope, still necessary in 25.11, albeit a different version.
  nixpkgs.config.permittedInsecurePackages = [
    "beekeeper-studio-5.3.4"
  ];
}
