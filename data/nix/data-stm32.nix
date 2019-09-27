self: super:
{
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
      (hself: hsuper: {
        data-stm32 = super.haskell.lib.dontHaddock ( hself.callCabal2nix "data-stm32" ../.. {} );
      });
    });
}
