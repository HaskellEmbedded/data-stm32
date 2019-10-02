self: super:
let

  # update with
  # nix-prefetch-git https://github.com/sorki/ivory-tower-stm32 <OPTIONAL_REV>
  src = super.fetchFromGitHub {
    owner = "sorki";
    repo = "ivory-tower-stm32";
    rev = "4d1f5510d831bb5ba3ef8f628286547f4fa37250";
    sha256 = "0nq6z91bw2l0knirfmjmawplfagwbn13mmwclfpsa5l6vhmwcqx4";
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
