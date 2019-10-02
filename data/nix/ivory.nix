self: super:
let
  src = super.fetchFromGitHub {
    owner = "sorki";
    repo = "ivory";
    rev = "bf9f7d8c5c60b7842572acb2dbb6927d698c8e27";
    sha256 = "1mfrjgf3izwq7dvxj6rgxi7g5b30i3l85aqavbjll7vrmj40ny7h";
  };

  srcX = ../../../ivory;
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
