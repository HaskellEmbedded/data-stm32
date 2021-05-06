{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8101" }:

let
  itnSrc = nixpkgs.fetchFromGitHub {
    owner = "HaskellEmbedded";
    repo = "ivory-tower-nix";
    rev = "ccc4d98569d78c331df26bf9f387a7eb52c0c068";
    sha256 = "1s2havlknh4dpc5q45i5c1cbp8j4kk9zj23d9br2v2qwa49d536g";
  };
  itn = import itnSrc { inherit compiler nixpkgs; };
  src = itn.pkgs.nix-gitignore.gitignoreSource [] ./.;
in
itn.pkgs.haskell.lib.dontCheck
  (itn.ivorypkgs.callCabal2nix "data-stm32" src {})
