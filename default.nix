{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc864" }:
nixpkgs.haskell.packages.${compiler}.callPackage ./data-stm32.nix { }
