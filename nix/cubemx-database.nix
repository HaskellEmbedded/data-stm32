{ stdenv, fetchurl, unzip }:
stdenv.mkDerivation rec {
  name = "cubemx-database";

  src = fetchurl (import ./cmx/cmx.nix);

  buildInputs = [ unzip ];

  unpackPhase = ''
    unzip ${src}
  '';

  installPhase = ''
    mkdir -p $out/
    cp -a MX/db/* $out/
  '';
}
