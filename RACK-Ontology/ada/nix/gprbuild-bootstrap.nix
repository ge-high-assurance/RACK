{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, gnat ? nixpkgs.gnat10
}:
let
  gprconfig_kb-source = import ./gprconfig_kb-source.nix { inherit nixpkgs sources; };
  xmlada-source = import ./xmlada-source.nix { inherit nixpkgs sources; };
in
nixpkgs.callPackage ./gprbuild-bootstrap {
  inherit gnat gprconfig_kb-source nixpkgs sources xmlada-source;
}
