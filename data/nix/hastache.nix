self: super:
{
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
      (hself: hsuper: {
        hastache = hself.callPackage ../../nix/hastache.nix {};
      });
    });
}
