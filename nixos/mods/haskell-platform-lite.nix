{ pkgs, ... }: {
  environment.systemPackages = [
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
