{ nixpkgs, sources }:
nixpkgs.stdenv.mkDerivation {

  buildPhase = "echo SKIPPED";

  configurePhase = "echo SKIPPED";

  installPhase = ''
    mkdir -p $out
    cp -r ./* $out/
  '';

  name = "xmlada-source";
  src = fetchTarball { inherit (sources.xmlada) url sha256; };
}
