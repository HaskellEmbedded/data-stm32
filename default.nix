{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc864" }:
nixpkgs.haskell.lib.overrideCabal
  (nixpkgs.haskell.packages.${compiler}.callPackage ./data-stm32.nix { })
  ( oldDrv: {
      src = nixpkgs.nix-gitignore.gitignoreSource [] ./.;
    }
)
