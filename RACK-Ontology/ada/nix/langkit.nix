{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, pythonPackages ? nixpkgs.python37Packages
}:
let
  e3-core = import ./e3-core.nix { inherit nixpkgs pythonPackages sources; };
  # gnatcoll-core = import ./gnatcoll-core.nix { inherit nixpkgs sources; };
  # gprbuild = import ./gprbuild.nix { inherit nixpkgs sources; };
  # libgpr = import ./libgpr.nix { inherit nixpkgs sources; };
  # xmlada = import ./xmlada.nix { inherit nixpkgs sources; };
in
nixpkgs.callPackage ./langkit {
  inherit
    e3-core
    nixpkgs
    sources
  ;
  inherit (pythonPackages)
    buildPythonPackage
    docutils
    funcy
    Mako
  ;
}
