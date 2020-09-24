{ buildPythonPackage, colorama, fetchPypi, nixpkgs, psutil
, python-dateutil, pyyaml, requests-toolbelt, sources, stevedore, tqdm
}:
buildPythonPackage rec {

  pname = "e3-core";
  version = "22.1.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "0cqbzcq606fbqf10wv1fsfw894lwzlhpm3n9haljmiayliydw3d9";
  };

  # NOTE (val) this is giving me trouble, not sure whether needed
  postPatch = ''
    substituteInPlace setup.py \
      --replace "        extras_require[platform_string].append(\"ld\")" \
      "        pass"
  '';

  doCheck = false;

  propagatedBuildInputs = [
    colorama
    psutil
    python-dateutil
    pyyaml
    requests-toolbelt
    stevedore
    tqdm
  ];

}
