{ gcc-build-output, gcc-source, glibc, gnat, gnatcoll-core, gprbuild, libgpr, nixpkgs, sources, xmlada
}:
nixpkgs.stdenv.mkDerivation {

  buildInputs = [
      gnat
      gprbuild
  ];

  # LIBRARY_PATH is needed so that ld can find crti.o and crt1.o
  configurePhase = ''
    export RELEASE=${gnat.version}
    echo $RELEASE
    export GCC_SRC_BASE=${gcc-source}
    export GCC_BLD_BASE=${gcc-build-output}/build
    export LIBRARY_PATH="${glibc}/lib"
  '';

  buildPhase = ''
    make
  '';

  installPhase = ''
    make install prefix=$out
  '';

  name = "gnat_util";
  src = fetchTarball { inherit (sources.gnat_util) url sha256; };

}
