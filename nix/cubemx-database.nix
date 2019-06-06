{ nixpkgs ? import <nixpkgs> {} }:
let
  izpack = nixpkgs.callPackage ./modm-devices-izpack.nix {};
  cmxRes = nixpkgs.callPackage ./cubemx-core-resource.nix {};
in
  nixpkgs.runCommand "cubemx-database" {} ''
    mkdir tmp
    ln -s ${cmxRes}/resources .

    pushd tmp
    ${izpack}/bin/izpack
    popd

    mv output/db/ $out
  ''
