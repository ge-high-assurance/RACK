{ nixpkgs ? import <nixpkgs> {}
}:
let
  gnatcoll-bindings-gmp = import ./nix/gnatcoll-bindings-gmp.nix {};
  gnatcoll-bindings-iconv = import ./nix/gnatcoll-bindings-iconv.nix {};
  gnatcoll-core = import ./nix/gnatcoll-core.nix {};
  gprbuild = import ./nix/gprbuild.nix {};
  libgpr = import ./nix/libgpr.nix {};
  myPythonPackages = nixpkgs.python38Packages;
  xmlada = import ./nix/xmlada.nix {};
in
nixpkgs.mkShell {
  buildInputs = with nixpkgs; [
    gnatcoll-bindings-gmp
    gnatcoll-bindings-iconv
    gnatcoll-core
    gprbuild
    myPythonPackages.pip
    myPythonPackages.setuptools
    myPythonPackages.virtualenv
  ];

  GPR_PROJECT_PATH="${gnatcoll-bindings-gmp}/share/gpr:${gnatcoll-bindings-iconv}/share/gpr:${gnatcoll-core}/share/gpr:${libgpr}/share/gpr:${xmlada}/share/gpr";

  LIBRARY_PATH="${nixpkgs.glibc}/lib";

  shellHook = ''
  alias pip="PIP_PREFIX='$(pwd)/_build/pip_packages' \pip"
  export PYTHONPATH="$(pwd)/_build/pip_packages/lib/python2.7/site-packages:$PYTHONPATH"
  unset SOURCE_DATE_EPOCH
  '';
}
