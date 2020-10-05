{ sources ? import ./nix/nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, pythonPackages ? nixpkgs.python37Packages
}:
let
  gnat = nixpkgs.gnat10;
  libadalang = import ./nix/libadalang.nix { inherit gnat nixpkgs pythonPackages; };
  stubs = import ./stubs { inherit nixpkgs; };
in
nixpkgs.mkShell {
  buildInputs = with nixpkgs; [
    libadalang
    pythonPackages.colorama
    pythonPackages.mypy
    pythonPackages.pylint
    stubs
  ];

  # for quick access to this offline documentation, if needed
  AST_HTML="${libadalang}/share/libadalang/ast-types.html";
  # necessary so that the linker finds the libadalang files
  LD_LIBRARY_PATH="${libadalang}/lib";
  # quick access to some Ada files, for testing purposes
  LIBADALANG_ADA_FILES="${libadalang}/include/libadalang";
  # variable used by mypy to find type signature stubs for libraries missing them
  MYPYPATH="${stubs}";

  shellHook = ''
    export PATH=${libadalang}/bin:$PATH
    export PYTHONPATH=${libadalang}/python:$PYTHONPATH
  '';
}
