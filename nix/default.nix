{ pkgs ? import <nixpkgs> {} }:
let
  cmxDb = pkgs.callPackage ./cubemx-database.nix {};
  svdDb = pkgs.callPackage ./svd-database.nix {};
in
  { inherit cmxDb svdDb;
  combined = pkgs.runCommand "data-stm32-db" {} ''
    mkdir $out
    ln -s ${cmxDb} $out/db
    ln -s ${svdDb} $out/svds
  ''; }
