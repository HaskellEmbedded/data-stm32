{ stdenv, fetchFromGitHub, jdk11 }:
stdenv.mkDerivation {
  name = "svd-database";

  # from svd branch, generated with gensvd
  src = fetchFromGitHub {
    owner = "HaskellEmbedded";
    repo = "data-stm32";
    rev = "a402da49a77cdd5ef81d0162e8a368f63ed2cb22";
    sha256 = "0jri7qnph5vcwi3jd0qqw0gkivpcbbbkyziixm5ihfbz7r53z8xg";
  };

  buildCommand = ''
    ln -s $src/svds $out
  '';
}
