{ buildPythonPackage, docutils, e3-core, funcy, Mako, nixpkgs, sources
}:

buildPythonPackage rec {
  pname = "langkit";
  version = "0.1-dev";

  buildInputs = with nixpkgs; [
  ];

  doCheck = false;

  propagatedBuildInputs = [
    docutils
    e3-core
    funcy
    Mako
  ];

  src = fetchTarball { inherit (sources.langkit) url sha256; };

}
