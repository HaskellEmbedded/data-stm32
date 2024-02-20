{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc963" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
