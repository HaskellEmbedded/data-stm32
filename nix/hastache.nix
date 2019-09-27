{ mkDerivation, base, blaze-builder, bytestring, containers
, directory, fetchgit, filepath, HUnit, ieee754, mtl, process
, stdenv, syb, text, transformers
}:
mkDerivation {
  pname = "hastache";
  version = "0.6.1";
  src = fetchgit {
    url = "https://github.com/basvandijk/hastache/";
    sha256 = "1a7hhdmr5jgn87l4nqkfh8nlcxamcya5g8p9rpb4dc7ycjllrbdc";
    rev = "ad4306a26f52e495f8da10c7928aa218b10d740e";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base blaze-builder bytestring containers directory filepath ieee754
    mtl syb text transformers
  ];
  executableHaskellDepends = [
    base blaze-builder bytestring containers directory filepath ieee754
    mtl process syb text transformers
  ];
  testHaskellDepends = [
    base bytestring directory HUnit mtl syb text
  ];
  homepage = "http://github.com/lymar/hastache";
  description = "Haskell implementation of Mustache templates";
  license = stdenv.lib.licenses.bsd3;
}
