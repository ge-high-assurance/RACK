{ glibc, gmp, gnatcoll-core, gprbuild, libgpr, nixpkgs, python, sources, which, xmlada-bootstrap
}:
nixpkgs.stdenv.mkDerivation {

  buildInputs = [
    gnatcoll-core
    gprbuild
    python
    which
  ];

  propagatedBuildInputs = [
    gmp
  ];

  configurePhase = ''
    export GPR_PROJECT_PATH="${gnatcoll-core}/share/gpr:${libgpr}/share/gpr:${xmlada-bootstrap}/share/gpr"
    export LIBRARY_PATH="${glibc}/lib"
  '';

  buildPhase = ''
    python ./gmp/setup.py build
  '';

  installPhase = ''
    python ./gmp/setup.py install --prefix $out
  '';

  name = "gnatcoll-bindings-gmp";
  src = fetchTarball { inherit (sources.gnatcoll-bindings) url sha256; };

}
