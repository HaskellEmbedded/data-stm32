{ stdenv, fetchFromGitHub, jdk11 }:
stdenv.mkDerivation {
  name = "modm-devices-izpack";

  src = fetchFromGitHub {
    owner = "modm-io";
    repo = "modm-devices";
    rev = "7f0b11d7e61b0a224c07ab0964c488aa11ecef1e";
    sha256 = "1mi14s5b2pdyklnfs8ryaaqsllkw0947ykclpj75zp77x3fv5s1y";
  };

  buildInputs = [ jdk11 ];
  buildCommand = ''
    cp -r $src/tools/generator/raw-data-extractor/izpack .
    chmod -R 755 izpack
    javac -encoding ISO-8859-1 izpack/*.java

    mkdir -p $out/bin
    mkdir -p $out/bin/izpack_deserializer
    mkdir -p $out/bin/com/izforge/izpack/api/data/

    mv izpack/IzPackDeserializer.class $out/bin/izpack_deserializer
    mv izpack/*.class $out/bin/com/izforge/izpack/api/data/

    cat > $out/bin/izpack <<EOF
      export CLASSPATH=$out/bin
      ${jdk11}/bin/java izpack_deserializer.IzPackDeserializer
    EOF
    chmod +x $out/bin/izpack
  '';
}
