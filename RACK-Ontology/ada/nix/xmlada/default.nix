{ glibc, gnat, gprbuild, nixpkgs, sources }:
nixpkgs.stdenv.mkDerivation {

  buildInputs = [
    gnat
    gprbuild
  ];

  configurePhase = ''
    export LIBRARY_PATH="${glibc}/lib"
    ./configure --prefix=$out BUILD_TYPE=Production
  '';

  buildPhase = ''
    make
  '';

  installPhase = ''
    make install
  '';

  name = "xmlada";
  src = fetchTarball { inherit (sources.xmlada) url sha256; };

}
