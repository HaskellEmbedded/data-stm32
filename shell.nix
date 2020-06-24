{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8101" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
