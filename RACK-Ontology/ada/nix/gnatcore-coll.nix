{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
}:
let
  gprbuild = import ./gprbuild.nix { inherit nixpkgs sources; };
  xmlada-bootstrap = import ./xmlada-bootstrap.nix { inherit nixpkgs sources; };
in
nixpkgs.callPackage ./gnatcore-coll {
  inherit
    gprbuild
    nixpkgs
    sources
    xmlada-bootstrap;
}
