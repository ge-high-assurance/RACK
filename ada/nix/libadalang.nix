{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, gnat ? nixpkgs.gnat10
, pythonPackages ? nixpkgs.python37Packages
}:
let
  asis-for-gnat = import ./asis-for-gnat.nix { inherit gnat nixpkgs sources; };
  gnatcoll-core = import ./gnatcoll-core.nix { inherit gnat nixpkgs sources; };
  gnatcoll-bindings-gmp = import ./gnatcoll-bindings-gmp.nix { inherit gnat nixpkgs sources; };
  gnatcoll-bindings-iconv = import ./gnatcoll-bindings-iconv.nix { inherit gnat nixpkgs sources; };
  gprbuild = import ./gprbuild.nix { inherit nixpkgs sources; };
  langkit = import ./langkit.nix { inherit nixpkgs pythonPackages sources; };
  libgpr = import ./libgpr.nix { inherit gnat nixpkgs sources; };
  xmlada = import ./xmlada.nix { inherit gnat nixpkgs sources; };
in
nixpkgs.callPackage ./libadalang {
  inherit
    asis-for-gnat
    gnat
    gnatcoll-bindings-gmp
    gnatcoll-bindings-iconv
    gnatcoll-core
    gprbuild
    langkit
    libgpr
    nixpkgs
    sources
    xmlada
  ;
  inherit (pythonPackages) black;
}
