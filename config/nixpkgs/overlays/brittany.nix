self: super: {
  myBrit = self.haskellPackages.extend (selfHS: superHS: {
    monad-memo = super.haskell.lib.appendConfigureFlag superHS.monad-memo "--ghc-option=-XFlexibleContexts";
  });
}
