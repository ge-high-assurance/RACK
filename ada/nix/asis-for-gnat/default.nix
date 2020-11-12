{ gcc-source, glibc, gnat, gnat_util, gnatcoll-core, gprbuild, libgpr, nixpkgs, sources, xmlada
}:
nixpkgs.stdenv.mkDerivation {

  buildInputs = [
      gnat
      gnat_util
      gprbuild
  ];

  # Windows way

  # configurePhase = ''
  # export GPR_PROJECT_PATH="${gcc-source}:${gnat_util}/lib/gnat:${libgpr}/share/gpr:${xmlada}/share/gpr"
  # export LIBRARY_PATH="${gcc-source}:${gcc-source}/gcc/ada"
  # '';

  # buildPhase = ''
  # gprbuild -vh -p -j0 -XBLD=prod -vl -XASIS_COMPONENTS=lib build_asis.gpr
  # '';

  # installPhase = ''
  # '';

  # LIBRARY_PATH is needed so that ld can find crti.o and crt1.o
  configurePhase = ''
    export GPR_PROJECT_PATH="${gnatcoll-core}/share/gpr:${gnat_util}/lib/gnat:${libgpr}/share/gpr:${xmlada}/share/gpr"
    export LIBRARY_PATH="${glibc}/lib:${gnat_util}/lib/gnat"
    make prefix=$out BUILD=production setup
  '';

  buildPhase = ''
    make all prefix=$out
  '';

  installPhase = ''
    make install prefix=$out
  '';

  name = "asis-for-gnat";
  src = fetchTarball { inherit (sources.ASIS) url sha256; };

}
