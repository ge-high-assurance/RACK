{ glibc, gnat, gnatcoll-core, gprbuild, libgpr, libiconv, nixpkgs, python, sources, which, xmlada-bootstrap
}:
nixpkgs.stdenv.mkDerivation {

  buildInputs = [
    gnat
    gnatcoll-core
    gprbuild
    python
    which
  ];

  propagatedBuildInputs = [
    libiconv
  ];

  configurePhase = ''
    export GPR_PROJECT_PATH="${gnatcoll-core}/share/gpr:${libgpr}/share/gpr:${xmlada-bootstrap}/share/gpr"
    export LIBRARY_PATH="${glibc}/lib"
  '';

  buildPhase = ''
    python ./iconv/setup.py build
  '';

  installPhase = ''
    python ./iconv/setup.py install --prefix $out
  '';

  name = "gnatcoll-bindings-iconv";
  src = fetchTarball { inherit (sources.gnatcoll-bindings) url sha256; };

}
