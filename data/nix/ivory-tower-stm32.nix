self: super:
let

  # update with
  # nix-prefetch-git https://github.com/sorki/ivory-tower-stm32 <OPTIONAL_REV>
  src = super.fetchFromGitHub {
    owner = "sorki";
    repo = "ivory-tower-stm32";
    rev = "8c795efcad335165e529110dca10860d37a93120";
    sha256 = "1a6cpa3qihhvkdrlvrv1xp7md2zncncrsb1q85qxykm0a4dp11mq";
  };

  # swap with src to build from this path
  srcX = ../../../ivory-tower-stm32;

  subs = [
    # "ivory-bsp-stm32"
    # "ivory-bsp-tests" # XXX: port this please
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
