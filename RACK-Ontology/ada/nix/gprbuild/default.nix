{ gnat, gprbuild-bootstrap, nixpkgs, sources, which, xmlada-bootstrap
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

  configurePhase = ''
    export GPR_PROJECT_PATH="${xmlada-bootstrap}/share/gpr"
  '';

  buildPhase = ''
    mkdir -p $out
    make prefix=$out setup
    # export PATH=./:$PATH
    make LIBRARY_TYPE=static all
  '';

  name = "gprbuild";
  src = fetchTarball { inherit (sources.gprbuild) url sha256; };
}
