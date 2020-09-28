{ sources ? import ./nix/nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
}:
let
  gnat = nixpkgs.gnat10;
  libadalang = import ./nix/libadalang.nix { inherit gnat nixpkgs; };
in
nixpkgs.mkShell {
  buildInputs = with nixpkgs; [
    # gnatcoll-bindings-gmp
    # gnatcoll-bindings-iconv
    # gnatcoll-core
    # gprbuild
    libadalang
    # myPythonPackages.pip
    # myPythonPackages.setuptools
    # myPythonPackages.virtualenv
  ];

  # GPR_PROJECT_PATH="${gnatcoll-bindings-gmp}/share/gpr:${gnatcoll-bindings-iconv}/share/gpr:${gnatcoll-core}/share/gpr:${libgpr}/share/gpr:${xmlada}/share/gpr";

  # LIBRARY_PATH="${nixpkgs.glibc}/lib";

  # shellHook = ''
  # alias pip="PIP_PREFIX='$(pwd)/_build/pip_packages' \pip"
  # export PYTHONPATH="$(pwd)/_build/pip_packages/lib/python2.7/site-packages:$PYTHONPATH"
  # unset SOURCE_DATE_EPOCH
  # '';
}
