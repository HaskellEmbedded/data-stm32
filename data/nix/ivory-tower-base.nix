self: super:
let
  # update with
  # nix-prefetch-git https://github.com/distrap/ivory-tower-base <OPTIONAL_REV>
  src = super.fetchFromGitHub {
    owner = "distrap";
    repo = "ivory-tower-base";
    rev = "ae996126761bc90c2bc4f5b8c22990d83b956251";
    sha256 = "0dmkxgz17s4zlpjkg7iqql26vdc8qlahw9kaq4xc637dii69nykd";
  };

  # swap with src to build from this path
  srcX = ../../../ivory-tower-base;
in
{
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
      (hself: hsuper: {
        ivory-tower-base = super.haskell.lib.dontHaddock ( hself.callCabal2nix "ivory-tower-base" "${src}" {} );
      });
    });
}
