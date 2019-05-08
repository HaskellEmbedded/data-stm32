self: super:
let
   src = super.fetchFromGitHub {
    owner = "GaloisInc";
    repo = "ivory";
    rev = "6cf27e193097fceacdb3270dbd63371cc871447c";
    sha256 = "13kanlylp9adi1p1j2h2rnfizh0bx5db63qyzmbls7ywrpbr0j0s";
  };
  subs = [
    "ivory"
    "ivory-artifact"
    "ivory-backend-c"
    "ivory-eval"
    "ivory-examples"
    "ivory-hw"
    "ivory-model-check"
    "ivory-opts"
    "ivory-quickcheck"
    "ivory-serialize"
    "ivory-stdlib"
  ];
in
{
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
      (hself: hsuper: super.lib.genAttrs subs (sub: hself.callCabal2nix sub "${src}/${sub}" {}));
    });
}
