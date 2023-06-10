{ pkgs, ... }: {
  environment.systemPackages = [
    pkgs.cabal-install
    pkgs.cabal2nix
    pkgs.haskellPackages.eventlog2html
    pkgs.haskellPackages.fast-tags
    pkgs.haskellPackages.fourmolu
    pkgs.haskellPackages.ghc-events
    pkgs.haskellPackages.hasktags
    pkgs.hlint
    pkgs.hpack
    pkgs.stack
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [
      QuickCheck
      containers
      criterion
      hspec
      lens
      megaparsec
      nonempty-containers
      nonempty-vector
      placeholders
      pretty-simple
      regex-applicative
      req
      scalpel-core
      servant
      split
      tasty
      tasty-hunit
      tasty-hspec
      tasty-quickcheck
      template-haskell
      text
      turtle
    ]))
  ];
}
