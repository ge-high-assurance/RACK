{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, pythonPackages ? nixpkgs.python37Packages
}:
let
in
nixpkgs.callPackage ./e3-core {
  inherit
    nixpkgs
    sources
  ;
  inherit (pythonPackages)
    buildPythonPackage
    colorama
    fetchPypi
    psutil
    python-dateutil
    pyyaml
    requests-toolbelt
    stevedore
    tqdm
  ;
}
