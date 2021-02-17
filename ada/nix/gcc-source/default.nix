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
    sha256 = "130xdkhmz1bc2kzx061s3sfwk36xah1fw5w332c0nzwwpdl47pdq";
  };

}
