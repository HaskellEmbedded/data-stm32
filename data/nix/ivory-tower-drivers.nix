self: super:
let

  # update with
  # nix-prefetch-git https://github.com/distrap/ivory-tower-drivers <OPTIONAL_REV>
  src = super.fetchFromGitHub {
    owner = "distrap";
    repo = "ivory-tower-drivers";
    rev = "7095e1fcd1b536b418ae3cec420d495053f7591c";
    sha256 = "124rwcvbm2syix35y12m479pdjsbrqkg20r712mbhkk4h3zrlrhg";
  };

  # swap with src to build from this path
  srcX = ../../../ivory-tower-drivers;
  # XXX: we could possibly provide override function
  # for this in default.nix
in
{
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
      (hself: hsuper: {
        ivory-tower-drivers = super.haskell.lib.dontHaddock ( hself.callCabal2nix "ivory-tower-drivers" "${src}" {} );
      });
    });
}
