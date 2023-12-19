{ stdenv
, bash
, fetchFromGitHub
, python3
, svdtools
, unzip
}:
stdenv.mkDerivation {
  name = "stm32-rs-svd-database";

  # from svd branch, generated with gensvd
  src = fetchFromGitHub {
    owner = "stm32-rs";
    repo = "stm32-rs";
    rev = "957a00e67fba56a71b674c143fd0dc1e007dbbba";
    sha256 = "08cl6aqq2ql5f9kgdpzpbyilphj56l7wnjb4di3ix8w48gz8sf97";
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
