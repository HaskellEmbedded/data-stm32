{ stdenv, fetchFromGitHub, jdk11 }:
stdenv.mkDerivation {
  name = "svd-database";

  # from svd branch, generated with gensvd
  src = fetchFromGitHub {
    owner = "HaskellEmbedded";
    repo = "data-stm32";
    rev = "e2d647e6c11c7df519af3009128a922a6ebe55cb";
    sha256 = "11z0bwpmsan4yrfasyqw5g6jjd5265hp0v79l2nhmkhn802gf2pz";
  };

  buildCommand = ''
    ln -s $src/svds $out
  '';
}
