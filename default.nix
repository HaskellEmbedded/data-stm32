{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
let
  hastache = nixpkgs.haskell.packages.${compiler}.callPackage ./nix/hastache.nix {};
in
nixpkgs.haskell.lib.overrideCabal
  (nixpkgs.haskell.packages.${compiler}.callPackage ./data-stm32.nix { inherit hastache; })
  ( oldDrv: {
      src = nixpkgs.nix-gitignore.gitignoreSource [] ./.;
    }
)
