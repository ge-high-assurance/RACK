{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, gnat ? nixpkgs.gnat10
}:
let
  gnat_util = import ./gnat_util.nix { inherit gnat nixpkgs sources; };
  gnatcoll-core = import ./gnatcoll-core.nix { inherit gnat nixpkgs sources; };
  gprbuild = import ./gprbuild.nix { inherit gnat nixpkgs sources; };
  libgpr = import ./libgpr.nix { inherit gnat nixpkgs sources; };
  xmlada = import ./xmlada.nix { inherit gnat nixpkgs sources; };
in
nixpkgs.callPackage ./asis-for-gnat {
  inherit
    gnat
    gnat_util
    gnatcoll-core
    gprbuild
    libgpr
    nixpkgs
    sources
    xmlada
  ;
}
