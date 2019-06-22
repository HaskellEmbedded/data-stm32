self: super:
let
  src = super.fetchFromGitHub {
    owner = "sorki";
    repo = "ivory-tower-stm32";
    rev = "2e40347c38ab4470e5265c243770e186c16c340a";
    sha256 = "1k9fxsw7x697g8b2whq0q32lp4mw38yq176jxxnaqmj1r6yyk0dp";
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
