{ glibc, gnat, gprbuild, nixpkgs, sources, which, xmlada-bootstrap
}:
nixpkgs.stdenv.mkDerivation {

  buildInputs = [
    gprbuild
    which
    xmlada-bootstrap
  ];

  propagatedBuildInputs = [
    gnat
  ];

  configurePhase = ''
    export GPR_PROJECT_PATH="${xmlada-bootstrap}/share/gpr"
    export LIBRARY_PATH="${glibc}/lib"
  '';

  buildPhase = ''
    make prefix=$out setup
    make
  '';

  name = "gnatcoll-core";
  src = fetchTarball { inherit (sources.gprbuild) url sha256; };

}
