self: super:
let
   src = super.fetchFromGitHub {
    owner = "mmilata";
    repo = "ivory-tower-stm32";
    rev = "f4ccc67fa4bef6e4e2bef987ee9241ab4ccb588c";
    sha256 = "1sxwf2n7j06j74kwqap8hl5v5976pcdccnv5ry073xf1d27409if";
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
