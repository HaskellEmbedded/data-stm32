{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8101" }:

let
  ivory-tower-nix = nixpkgs.fetchFromGitHub {
    owner = "HaskellEmbedded";
    repo = "ivory-tower-nix";
    rev = "9ffb1710aff73a43e5059e5c718327bd3f644bac";
    sha256 = "0nrm0jax6iwjm78paw7jrq31jyi8kpd5qkaxl2dwvhpcv1n3ydpp";
  };

  overlays = (import "${ivory-tower-nix}/overlay.nix" compiler);
  pkgs = import <nixpkgs> { inherit overlays; };
in
pkgs.haskell.lib.overrideCabal
  (pkgs.myHaskellPackages.callPackage ./data-stm32.nix { })
  ( oldDrv: {
      doCheck = false;
      src = pkgs.nix-gitignore.gitignoreSource [] ./.;
    }
)
