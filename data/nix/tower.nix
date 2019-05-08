self: super:
let
   src = super.fetchFromGitHub {
    owner = "sorki";
    repo = "tower";
    rev = "cd34214adb7dd68ea33bb1e865ca6418974fe3a6";
    sha256 = "1ww6922ii96v9hr6w3b4p9zacgmd7akgs1p8kdd6gg6y1jp85ydk";
  };

  subs = [
    "tower"
    "tower-aadl"
    "tower-config"
    "tower-hal"
    "tower-mini"
  ];
in
{
  haskellPackages = super.haskellPackages.override (old: {
    overrides = super.lib.composeExtensions (old.overrides or (_: _: {}))
      (hself: hsuper: super.lib.genAttrs subs (sub: hself.callCabal2nix sub "${src}/${sub}" {}));
    });
}
