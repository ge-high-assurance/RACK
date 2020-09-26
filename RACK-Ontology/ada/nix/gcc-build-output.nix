{ sources ? import ./nix/sources.nix {}
, nixpkgs ? import sources.nixpkgs {}
, gnat ? nixpkgs.gnat10
}:
# This derivation is exactly like gcc, but preserves its build state in a build/
# directory because gnat_util needs it for some reason...
gnat.cc.overrideAttrs ({...}: {
  postBuild = ''
  mkdir -p $out/build
  cp -r ./. $out/build
  '';
})
