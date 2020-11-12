{ glibc, gnat, gprbuild-bootstrap, nixpkgs, sources }:
nixpkgs.stdenv.mkDerivation {

  buildInputs = [
    gnat
    gprbuild-bootstrap
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

  name = "xmlada-bootstrap";
  src = fetchTarball { inherit (sources.xmlada) url sha256; };

}
