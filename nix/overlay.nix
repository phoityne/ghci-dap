final: prev:
with final.haskell.lib;
with final.lib; {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
      self: super: {
        ghci-dap = buildFromSdist (self.callPackage ../default.nix {});
      }
    );
  });
}
