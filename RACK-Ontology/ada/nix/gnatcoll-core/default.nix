{ glibc, gnat, gprbuild, libgpr, nixpkgs, sources, which, xmlada-bootstrap
}:
nixpkgs.stdenv.mkDerivation {

  buildInputs = [
    gprbuild
    libgpr
    which
  ];

  propagatedBuildInputs = [
    gnat
  ];

  configurePhase = ''
    export GPR_PROJECT_PATH="${gprbuild}/share/gpr:${libgpr}/share/gpr:${xmlada-bootstrap}/share/gpr"
    export LIBRARY_PATH="${glibc}/lib"
  '';

  buildPhase = ''
    make prefix=$out setup
    make
  '';

  name = "gnatcoll-core";
  src = fetchTarball { inherit (sources.gnatcoll-core) url sha256; };

}
