{ nixpkgs ? import <nixpkgs> {} }:
let
  cmxDb = import ./cubemx-database.nix {};
  svdDb = nixpkgs.callPackage ./svd-database.nix {};
in
  nixpkgs.runCommand "env" {} ''
    mkdir $out
    ln -s ${cmxDb} $out/db
    ln -s ${svdDb} $out/svds
  ''
