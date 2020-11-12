{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, gnat ? nixpkgs.gnat10
}:
let
  gprbuild = import ./gprbuild.nix { inherit gnat nixpkgs sources; };
in
nixpkgs.callPackage ./xmlada {
  inherit gnat gprbuild nixpkgs sources;
}
