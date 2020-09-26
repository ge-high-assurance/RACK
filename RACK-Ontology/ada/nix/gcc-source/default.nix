{ gnat, nixpkgs, sources
}:
nixpkgs.stdenv.mkDerivation {

  configurePhase = ''
    # do nothing
  '';

  buildPhase = ''
    # do nothing
  '';

  installPhase = ''
    cp -r ./. $out
  '';

  fixupPhase = ''
    # do nothing
  '';

  name = "gcc-${gnat.cc.version}-source";
  src = nixpkgs.fetchurl {
    url = "mirror://gcc/releases/gcc-${gnat.cc.version}/gcc-${gnat.cc.version}.tar.xz";
    sha256 = "1817nc2bqdc251k0lpc51cimna7v68xjrnvqzvc50q3ax4s6i9kr";
  };

}
