{ mkDerivation, ansi-wl-pprint, attoparsec, base, bytestring
, cereal, containers, Diff, foldl, ghcid, hxt, hxt-xpath
, pretty-simple, regex-posix, split, stdenv, text, time, turtle
, vector
}:
mkDerivation {
  pname = "data-stm32";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint attoparsec base bytestring cereal containers Diff
    foldl ghcid hxt hxt-xpath pretty-simple regex-posix split text time
    turtle vector
  ];
  executableHaskellDepends = [
    ansi-wl-pprint attoparsec base bytestring cereal containers foldl
    hxt hxt-xpath pretty-simple regex-posix split text time turtle
    vector
  ];
  testHaskellDepends = [
    ansi-wl-pprint attoparsec base bytestring cereal containers foldl
    hxt hxt-xpath pretty-simple regex-posix split text time turtle
    vector
  ];
  homepage = "https://github.com/sorki/data-stm32#readme";
  description = "ARM SVD and CubeMX XML parser and pretty printer for STM32 family";
  license = stdenv.lib.licenses.bsd3;
}
