{ glibc, gnatcoll-core, gprbuild, libgpr, nixpkgs, sources, xmlada
}:
nixpkgs.stdenv.mkDerivation {

  buildInputs = [
      gprbuild
  ];

  # LIBRARY_PATH is needed so that ld can find crti.o and crt1.o
  configurePhase = ''
    export GPR_PROJECT_PATH="${gnatcoll-core}/share/gpr:${libgpr}/share/gpr:${xmlada}/share/gpr"
    export LIBRARY_PATH="${glibc}/lib"
  '';

  buildPhase = ''
    make
    exit 42
  '';

  installPhase = ''
    make install prefix=$out
  '';

  name = "asis-for-gnat";
  src = fetchTarball { inherit (sources.gnat_util) url sha256; };

}
