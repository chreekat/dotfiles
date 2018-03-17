self: super: {
  profiledHaskellPackages = self.haskellPackages.override {
    overrides = selfHS: superHS: {
      mkDerivation = args: superHS.mkDerivation (args // {
        enableLibraryProfiling = true;
      });
    };
  };
}

