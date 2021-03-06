{ nixpkgs ? import <nixpkgs> {}
}:
nixpkgs.stdenv.mkDerivation {
    name = "arcos-python-typing-stubs";
    src = nixpkgs.lib.sourceFilesBySuffices ./. [".pyi"];
    phases = ["unpackPhase" "installPhase"];
    installPhase = ''
    cp -r ./. $out
    '';
}
