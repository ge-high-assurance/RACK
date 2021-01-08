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
  ];

  postVenvCreation = ''
    pip install -r ${./requirements.txt}
  '';

  venvDir = ".venv";

}
