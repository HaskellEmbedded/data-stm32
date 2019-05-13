self: super:
let
   src = super.fetchFromGitHub {
    owner = "sorki";
    repo = "ivory-tower-stm32";
    rev = "01661d12d8b3084dc8f026a1c25442d16e661346";
    sha256 = "1f1d3fhsm7bh12rblcbr3lv5x95aid565h42s163sgv18pl1k7p4";
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
