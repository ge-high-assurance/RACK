{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, pythonPackages ? nixpkgs.python37Packages
}:
let
  e3-core = import ./e3-core.nix { inherit nixpkgs pythonPackages sources; };
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
