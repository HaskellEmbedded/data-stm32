{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc881" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
