{ gprbuild, nixpkgs, python38, python38Packages, sources
}:
let
  pythonPackages = python38Packages;
in
nixpkgs.stdenv.mkDerivation {

  buildInputs = [
    gprbuild
    python38
    python38Packages.pip
    python38Packages.virtualenv
  ];

  configurePhase = ''
    python -mvenv .env
    source ./.env/bin/activate
    pip install -r REQUIREMENTS.dev
    python ada/manage.py generate
  '';

  buildPhase = ''
    python ada/manage.py --library-types=static,static-pic,relocatable make --build-mode prod $out
  '';

  installPhase = ''
    python ada/manage.py --library-types=static,static-pic,relocatable install --build-mode prod $out
  '';

  name = "libadalang";
  src = fetchTarball { inherit (sources.libadalang) url sha256; };

}
