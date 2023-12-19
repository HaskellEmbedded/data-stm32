{ stdenv, fetchFromGitHub }:
stdenv.mkDerivation {
  name = "svd-database";

  # from svd branch, generated with gensvd
  src = fetchFromGitHub {
    owner = "HaskellEmbedded";
    repo = "data-stm32";
    rev = "258c812c1c3d37d7f82144516742d080f4ba19ac";
    sha256 = "18vlh41q8fh7mxnl88xfhrgbzsyazxhvp2l6fp3n6jc37mm3s7bq";
  };

  buildCommand = ''
    ln -s $src/svds $out
  '';
}
