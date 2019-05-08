{ stdenv, fetchurl, unzip }:
stdenv.mkDerivation rec {
  name = "cubemx-core-resource";

  src = fetchurl (import ./cmx/cmx.nix);

  buildInputs = [ unzip ];

  unpackPhase = ''
    unzip ${src}
  '';

  buildPhase = ''
    unzip -d ex SetupSTM32CubeMX*.exe || true
    chmod -R 755 ex
  '';

  installPhase = ''
    mkdir -p $out/resources/packs/
    cp ex/resources/packs/pack-Core $out/resources/packs
  '';
}
