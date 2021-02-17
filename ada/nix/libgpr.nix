{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, gnat ? nixpkgs.gnat10
}:
let
  gprbuild = import ./gprbuild.nix { inherit gnat nixpkgs sources; };
  gprconfig_kb-source = import ./gprconfig_kb-source.nix { inherit nixpkgs sources; };
  xmlada = import ./xmlada.nix { inherit gnat nixpkgs sources; };
in
nixpkgs.callPackage ./libgpr {
  inherit
    gnat
    gprconfig_kb-source
    gprbuild
    nixpkgs
    sources
    xmlada;
}
