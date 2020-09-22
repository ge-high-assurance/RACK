{ gnat, gprbuild-bootstrap, nixpkgs, sources, which, xmlada-bootstrap
}:
nixpkgs.gccStdenv.mkDerivation {

  buildInputs = [
    gprbuild-bootstrap
    which
    xmlada-bootstrap
  ];

  propagatedBuildInputs = [
    gnat
  ];

  configurePhase = ''
    export GPR_PROJECT_PATH="${xmlada-bootstrap}/share/gpr"
  '';

  name = "gnatcoll-core";
  src = fetchTarball { inherit (sources.gprbuild) url sha256; };
}
