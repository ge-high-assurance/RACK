{ asis-for-gnat, black, glibc, gmp, gnat, gnatcoll-bindings-gmp, gnatcoll-bindings-iconv
, gnatcoll-core, gprbuild, langkit, libgpr, nixpkgs, pythonPackages, sources, xmlada
}:
nixpkgs.stdenv.mkDerivation {

  buildInputs = [
    # asis-for-gnat
    gmp
    gnat
    gprbuild
  ];

  propagatedBuildInputs = [
    black
    langkit
  ];

  configurePhase = ''
    export GPR_PROJECT_PATH="${gnatcoll-core}/share/gpr:${gnatcoll-bindings-gmp}/share/gpr:${gnatcoll-bindings-iconv}/share/gpr:${libgpr}/share/gpr:${xmlada}/share/gpr"
    export LIBRARY_PATH="${glibc}/lib"
    python ada/manage.py generate
  '';

  buildPhase = ''
    python ada/manage.py --library-types=static,static-pic,relocatable make --build-mode prod
  '';

  installPhase = ''
    python ada/manage.py --library-types=static,static-pic,relocatable install --build-mode prod $out
  '';

  name = "libadalang";
  src = fetchTarball { inherit (sources.libadalang) url sha256; };

}
