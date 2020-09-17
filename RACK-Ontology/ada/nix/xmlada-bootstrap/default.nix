{ gprbuild-bootstrap, nixpkgs, sources }:
nixpkgs.stdenv.mkDerivation {

  buildInputs = [
    gprbuild-bootstrap
  ];

  configurePhase = ''
    ./configure --prefix=$out
  '';

  # NOTE (val) Getting linker errors when trying to build the dynamic one, so
  # trying to see if static is enough
  buildPhase = ''
    make static
  '';

  installPhase = ''
    mkdir -p $out
    make install-static
  '';

  name = "xmlada";
  src = fetchTarball { inherit (sources.xmlada) url sha256; };
}
