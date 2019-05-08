{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc864" }:
let
  overlays = [
    (import ./nix/ivory.nix)
    (import ./nix/ivory-tower-stm32.nix)
    (import ./nix/tower.nix)

    (import ./nix/generated.nix)
  ];
  pkgs = import <nixpkgs> { inherit overlays; };
in
pkgs.haskellPackages.ivory-bsp-stm32
