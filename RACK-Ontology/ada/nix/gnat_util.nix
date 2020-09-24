{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
}:
let
  gnatcoll-core = import ./gnatcoll-core.nix { inherit nixpkgs sources; };
  gprbuild = import ./gprbuild.nix { inherit nixpkgs sources; };
  libgpr = import ./libgpr.nix { inherit nixpkgs sources; };
  xmlada = import ./xmlada.nix { inherit nixpkgs sources; };
in
nixpkgs.callPackage ./gnat_util {
  inherit
    gnatcoll-core
    gprbuild
    libgpr
    nixpkgs
    sources
    xmlada
  ;
}
