{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, gnat ? nixpkgs.gnat10
}:
let
  gprbuild-bootstrap = import ./gprbuild-bootstrap.nix { inherit gnat nixpkgs sources; };
  gprconfig_kb-source = import ./gprconfig_kb-source.nix { inherit nixpkgs sources; };
  xmlada-bootstrap = import ./xmlada-bootstrap.nix { inherit gnat nixpkgs sources; };
in
nixpkgs.callPackage ./gprbuild {
  inherit
    gnat
    gprconfig_kb-source
    gprbuild-bootstrap
    nixpkgs
    sources
    xmlada-bootstrap;
}
