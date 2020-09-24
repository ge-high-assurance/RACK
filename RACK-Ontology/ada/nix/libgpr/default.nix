{ glibc, gnat, gprbuild-bootstrap, gprconfig_kb-source, nixpkgs, sources, which, xmlada-bootstrap
}:
nixpkgs.stdenv.mkDerivation {

  buildInputs = [
    gprbuild-bootstrap
    which
    xmlada-bootstrap
  ];

  propagatedBuildInputs = [
    gnat
  ];

  # LIBRARY_PATH is needed so that ld can find crti.o and crt1.o
  configurePhase = ''
    export GPR_PROJECT_PATH="${xmlada-bootstrap}/share/gpr"
    export LIBRARY_PATH="${glibc}/lib"
    make prefix=$out BUILD=production setup
  '';

  buildPhase = ''
    make libgpr.build
  '';

  # NOTE (val) for some reason `make install` does not copy the gprconfig
  # knowledge database in the output, which causes problems when running
  # gprbuild.  So doing it manually here.
  installPhase = ''
    mkdir -p $out
    make libgpr.install
  '';

  name = "libgpr";
  src = fetchTarball { inherit (sources.gprbuild) url sha256; };

}
