{ nixpkgs ? import <nixpkgs> {}
}:
let
  myPythonPackages = nixpkgs.python39Packages;
in
nixpkgs.mkShell {

  buildInputs = with nixpkgs; [
    git
    myPythonPackages.python
    myPythonPackages.venvShellHook
    myPythonPackages.wheel
  ];

  # Python wheel creation uses zip, but zip does not support timestamps prior
  # to 1980.  Since epoch 0 is prior to that, we set the epoch to 1980.
  postVenvCreation = ''
    export SOURCE_DATE_EPOCH=315532800
    pip install -r ${./requirements.txt}
    pip install -r ${./dev/requirements.txt}
  '';

  venvDir = ".venv";

}
