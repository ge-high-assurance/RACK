{ sources ? import ./nix/nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, pythonPackages ? nixpkgs.python37Packages
}:
let
  gnat = nixpkgs.gnat10;
  gprbuild = import ./nix/gprbuild.nix { inherit gnat nixpkgs; };
  libadalang = import ./nix/libadalang.nix { inherit gnat nixpkgs pythonPackages; };
  stubs = import ./stubs { inherit nixpkgs; };
  xmlada = import ./nix/xmlada.nix { inherit gnat nixpkgs; };
in
nixpkgs.mkShell {

  buildInputs = [
    nixpkgs.bash       # for using assist/databin wrappers
    gnat
    gprbuild
    libadalang
    pythonPackages.python
    pythonPackages.venvShellHook
    pythonPackages.wheel
    stubs
    xmlada
  ];

  # for quick access to this offline documentation, if needed
  AST_HTML="${libadalang}/share/libadalang/ast-types.html";
  # necessary so that the linker finds the libadalang files
  LD_LIBRARY_PATH="${libadalang}/lib";
  # quick access to some Ada files, for testing purposes
  LIBADALANG_ADA_FILES="${libadalang}/include/libadalang";
  LIBRARY_PATH="${xmlada}/lib";
  # variable used by mypy to find type signature stubs for libraries missing them
  MYPYPATH="${stubs}";

  shellHook = ''
    export PATH=${toString ./.}/venv/bin:${nixpkgs.glibc}/lib:${libadalang}/bin:$PATH
    export PYTHONPATH=${libadalang}/python:$PYTHONPATH
  '';

  # Python wheel creation uses zip, but zip does not support timestamps prior
  # to 1980.  Since epoch 0 is prior to that, we set the epoch to 1980.
  postVenvCreation = ''
    export SOURCE_DATE_EPOCH=315532800
    pip install -r ${./requirements.txt}
    pip install -r ${./requirements-dev.txt}
  '';

  venvDir = ".venv";

}
