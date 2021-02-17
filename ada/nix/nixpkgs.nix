{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
}:
nixpkgs
