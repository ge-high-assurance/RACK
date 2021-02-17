{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, gnat ? nixpkgs.gnat10
}:
nixpkgs.callPackage ./gcc-source {
  inherit
    gnat
    nixpkgs
    sources
  ;
}
