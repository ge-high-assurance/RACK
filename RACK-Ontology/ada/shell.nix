{ sources ? import ./nix/nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, pythonPackages ? nixpkgs.python37Packages
}:
let
  gnat = nixpkgs.gnat10;
  libadalang = import ./nix/libadalang.nix { inherit gnat nixpkgs pythonPackages; };
in
nixpkgs.mkShell {
  buildInputs = with nixpkgs; [
    libadalang
    pythonPackages.pylint
  ];

  AST_HTML="${libadalang}/share/libadalang/ast-types.html";

  LD_LIBRARY_PATH="${libadalang}/lib";

  shellHook = ''
    export PATH=${libadalang}/bin:$PATH
    export PYTHONPATH=${libadalang}/python:$PYTHONPATH
  '';
}
