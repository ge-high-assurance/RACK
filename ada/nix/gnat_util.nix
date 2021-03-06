{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, gnat ? nixpkgs.gnat10
}:
let
  gcc-build-output = import ./gcc-build-output.nix { inherit gnat nixpkgs sources; };
  gcc-source = import ./gcc-source.nix { inherit gnat nixpkgs sources; };
  gnatcoll-core = import ./gnatcoll-core.nix { inherit gnat nixpkgs sources; };
  gprbuild = import ./gprbuild.nix { inherit gnat nixpkgs sources; };
  libgpr = import ./libgpr.nix { inherit gnat nixpkgs sources; };
  xmlada = import ./xmlada.nix { inherit gnat nixpkgs sources; };
in
nixpkgs.callPackage ./gnat_util {
  inherit
    gcc-build-output
    gcc-source
    gnat
    gnatcoll-core
    gprbuild
    libgpr
    nixpkgs
    sources
    xmlada
  ;
}
