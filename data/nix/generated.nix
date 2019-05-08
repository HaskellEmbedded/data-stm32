self: super:
{
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
      (hself: hsuper: {
        ivory-bsp-stm32 = super.haskell.lib.dontHaddock ( hself.callPackage ../ivory-bsp-stm32.nix {} );
      });
    });
}
