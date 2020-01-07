{ mkDerivation, ansi-wl-pprint, attoparsec, base, bytestring
, cereal, containers, Diff, foldl, ghcid, hastache, hspec
, hspec-discover, hxt, hxt-xpath, karver, mtl, optparse-applicative
, pretty-simple, regex-posix, split, stdenv, syb, system-filepath
, text, time, turtle, unordered-containers, vector
}:
mkDerivation {
  pname = "data-stm32";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint attoparsec base bytestring cereal containers Diff
    foldl ghcid hastache hxt hxt-xpath karver pretty-simple regex-posix
    split text time turtle vector
  ];
  executableHaskellDepends = [
    ansi-wl-pprint attoparsec base bytestring cereal containers Diff
    foldl hastache hxt hxt-xpath karver mtl optparse-applicative
    pretty-simple regex-posix split syb text time turtle
    unordered-containers vector
  ];
  testHaskellDepends = [
    ansi-wl-pprint base bytestring hspec system-filepath
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/sorki/data-stm32#readme";
  description = "ARM SVD and CubeMX XML parser and pretty printer for STM32 family";
  license = stdenv.lib.licenses.bsd3;
}
