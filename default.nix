{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8101" }:

let
  itnSrc = nixpkgs.fetchFromGitHub {
    owner = "HaskellEmbedded";
    repo = "ivory-tower-nix";
    rev = "9ffb1710aff73a43e5059e5c718327bd3f644bac";
    sha256 = "0nrm0jax6iwjm78paw7jrq31jyi8kpd5qkaxl2dwvhpcv1n3ydpp";
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
