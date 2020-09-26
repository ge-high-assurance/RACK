{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, gnat ? nixpkgs.gnat10
}:
let
  gprbuild-bootstrap = import ./gprbuild-bootstrap.nix { inherit gnat nixpkgs sources; };
in
nixpkgs.callPackage ./xmlada-bootstrap {
  inherit gnat gprbuild-bootstrap nixpkgs sources;
}
