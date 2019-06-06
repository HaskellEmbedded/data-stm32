{ pkgs ? import <nixpkgs> {} }:
let
  dbs = import ./default.nix {};
  data-stm32 = import ../default.nix {};
in
pkgs.stdenv.mkDerivation {
  name = "data-stm32-environment";

  buildInputs = [
    pkgs.figlet
    data-stm32
  ];

  shellHook = ''
    figlet "YOLO"
    export DB_DIR=${dbs}
  '';
}

