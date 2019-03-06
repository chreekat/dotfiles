self: super: {
  haskellPackages = super.haskellPackages.override {
    overrides = selfHS: superHS: {
      mkDerivation = args: superHS.mkDerivation (args // {
        # doHaddock = false;
      });
      usort = import /home/b/Projects/usort;
      brittany = selfHS.callPackage /home/b/src/brittany
        { monad-memo =
            super.haskell.lib.appendConfigureFlag
              superHS.monad-memo
              "--ghc-option=-XFlexibleContexts"
        ; butcher = superHS.callHackage "butcher" "1.3.1.1" {}
        ; czipwith = superHS.callHackage "czipwith" "1.0.1.0" {}
        ;
        };
      hlint = selfHS.callCabal2nix "hlint" /home/b/src/hlint {};
      pomohoro = self.haskell.lib.doJailbreak superHS.pomohoro;
      #suavemente = superHS.callHackage "suavemente" "0.1.0.0" {};
    };
  };
}
