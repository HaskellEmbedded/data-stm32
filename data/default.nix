{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
let
  overlays = [
    (import ./nix/ivory.nix)
    (import ./nix/ivory-tower-stm32.nix)
    (import ./nix/tower.nix)

    (import ./nix/generated.nix)

    (import ./nix/ivory-tower-helloworld.nix)
    (import ./nix/ivory-tower-base.nix)
    (import ./nix/ivory-tower-drivers.nix)

    (import ./nix/hastache.nix)
    (import ./nix/data-stm32.nix)
  ];
  pkgs = import <nixpkgs> { inherit overlays; };
in
#pkgs.haskellPackages.ivory-bsp-stm32
pkgs.haskellPackages.ivory-tower-helloworld
