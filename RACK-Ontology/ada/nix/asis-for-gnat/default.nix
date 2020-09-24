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
    make prefix=$out BUILD=production setup
  '';

  buildPhase = ''
    make all
  '';

  installPhase = ''
    make install prefix=$out
  '';

  name = "asis-for-gnat";
  src = fetchTarball { inherit (sources.ASIS) url sha256; };

}
