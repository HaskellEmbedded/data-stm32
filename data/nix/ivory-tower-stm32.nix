self: super:
let
   src = super.fetchFromGitHub {
    owner = "GaloisInc";
    repo = "ivory-tower-stm32";
    rev = "350ec81025fd20c92d18934b15bc9caa5546caa2";
    sha256 = "0xgb2yfbzxbq5wyp1m2qs5vplpmwgdxryc26ncynbm0cibwiv3mz";
  };
  subs = [
    # "ivory-bsp-stm32"
    # "ivory-bsp-tests"
    "ivory-freertos-bindings"
    # "tower-echronos-stm32"
    "tower-freertos-stm32"
    # "tower-freertos-stm32-tests"
  ];
in
{
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
      (hself: hsuper: super.lib.genAttrs subs (sub: hself.callCabal2nix sub "${src}/${sub}" {}));
    });
}
