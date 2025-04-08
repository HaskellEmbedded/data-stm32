{ pkgs ? (import ./itn.nix {}).pkgs, ... }:
let
  cmxDb = pkgs.callPackage ./cubemx-database.nix {};
  svdDb = pkgs.callPackage ./stm32-rs-svds.nix {};
in
  { inherit cmxDb svdDb;
  combined = pkgs.runCommand "data-stm32-db" {} ''
    mkdir $out
    ln -s ${cmxDb} $out/db
    ln -s ${svdDb} $out/svds
  ''; }
