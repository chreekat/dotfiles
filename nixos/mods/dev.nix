{ pkgs, ...}:
{
  imports = [ ./bugs-b.nix ];
  documentation.dev.enable = true;

  environment.systemPackages = with pkgs; [
    act
    actionlint # GHA linter
    age
    (pkgs.callPackage "${builtins.fetchTarball "https://github.com/ryantm/agenix/archive/main.tar.gz"}/pkgs/agenix.nix" {})
    ssh-to-age
    bench
    bun
    beekeeper-studio
    cachix
    claude-code # Requires allowing non-free claude-code
    claude-monitor
    codex # Requires allowing non-free codex
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
    nixfmt
    nix-diff
    nix-prefetch
    nix-prefetch-docker
    nix-prefetch-github
    nix-prefetch-scripts
    nvd
    nodejs
    openssl
    python3
    shellcheck
    universal-ctags
    urlencode
    uv
    vim-full
    zgrviewer # graphviz, dot
  ] ++ [
    # packaging stuff

    # build.opensuse.org
    python3Packages.osc
    quilt

    # debian
    dput-ng
    dpkg
    debian-devscripts
  ] ++ [
    # Language servers (not Haskell, done separately)
    bash-language-server
    python3Packages.python-lsp-server
  ];

  programs.direnv.enable = true;

  programs.nix-ld.enable = true;

  nixpkgs.config.permittedInsecurePackages = [
    # Necessary in 25.05, but should be unnecessary in 25.11 Nope, still
    # necessary in 25.11, albeit a different version.
    "beekeeper-studio-5.3.4"
    # Necessary in 25.11
    "docker-28.5.2"
  ];
}
