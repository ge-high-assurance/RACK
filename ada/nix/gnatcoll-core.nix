{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, gnat ? nixpkgs.gnat10
}:
let
  gprbuild = import ./gprbuild.nix { inherit gnat nixpkgs sources; };
  libgpr = import ./libgpr.nix { inherit gnat nixpkgs sources; };
  xmlada-bootstrap = import ./xmlada-bootstrap.nix { inherit gnat nixpkgs sources; };
in
nixpkgs.callPackage ./gnatcoll-core {
  inherit
    gnat
    gprbuild
    libgpr
    nixpkgs
    sources
    xmlada-bootstrap
  ;
}
