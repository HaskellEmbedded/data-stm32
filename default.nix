{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8101" }:

let
  itnSrc = nixpkgs.fetchFromGitHub {
    owner = "HaskellEmbedded";
    repo = "ivory-tower-nix";
    rev = "00e5da396361978b791057555ed992d1118765d9";
    sha256 = "094dbs0vlaqi32x7m4qxz7xgjqi8hdhifnag9vfy9p3zwb817rh6";
  };
  itn = import itnSrc { inherit compiler nixpkgs; };
  src = itn.pkgs.nix-gitignore.gitignoreSource [] ./.;
in
itn.pkgs.haskell.lib.overrideCabal
  itn.ivorypkgs.data-stm32
  ( oldDrv: {
      doCheck = false;
      inherit src;
  })
