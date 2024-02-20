{ stdenv
, bash
, fetchFromGitHub
, python3
, svdtools
, unzip
}:
stdenv.mkDerivation {
  name = "stm32-rs-svd-database";

  # until
  # https://github.com/stm32-rs/stm32-rs/pull/941
  # https://github.com/stm32-rs/stm32-rs/pull/947
  src = fetchFromGitHub {
    owner = "sorki";
    repo = "stm32-rs";
    rev = "dbb6084d12930ecfd4f7ff338be6200d6e355dc9";
    sha256 = "13079g2kmb3rcmy8gf8v1sr5x20fsfqnk3dqs9v6a6xs77g8v383";
  };

  buildInputs = [ python3 svdtools unzip ];

  postPatch = ''
    patchShebangs .
  '';

  makeFlags = [ "SHELL=${bash}/bin/bash" "-j12" "patch" ];

  installPhase = ''
    pushd svd
    for i in $( ls *.svd ); do
      test -f $i.patched && mv -v $i.patched $i;
    done
    popd

    mkdir -p $out/stm
    cp svd/*.svd $out/stm
  '';
}
