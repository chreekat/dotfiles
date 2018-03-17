self: super: {
  haskellPackages = super.haskellPackages.override {
    overrides = selfHS: superHS: {
      usort = import /home/b/Projects/usort;
      #brittany = selfHS.callPackage /home/b/src/brittany/brittany.nix {};
    };
  };
}
