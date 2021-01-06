# Setup

The project is set up for easy use with `nix` and Python's built-in
`virtualenv`.

Using nix, you just need to enter the shell (using `nix-shell`) to be in an
environment with python and libadalang installed. To double-check, run the
`python` REPL and type `import libadalang`, it should succeed.

We recommend using `direnv` to automatically load the shell environment for you
whenever you enter this directory.

## Creating the virtual environment

You can run:

```shell
python3 -m venv ./venv
```

to create a directory `venv` with the necessary apparatus.

NOTE: For all the following commands, you can also put `./venv/bin` in your
`PATH` to avoid having to prefix the commands. If you choose to name your
virtual environment directory `venv`, the nix shell is actually set up to
have that directory in its `PATH` already.

You can then install the requirements in the virtual environment via:

```shell
./venv/bin/pip install -r requirements.txt
```

To run the analysis on the regression test (NOTE: this is likely to change
shortly, so this may not be up to date):

```shell
./venv/bin/python3 ./run_analysis.py --gpr ./example/regression/regression.gpr --analyze ./regression.adb
```

# Linting your code

The requirements also contain development dependencies like mypy and pylint.
You can run these in the expected way:

```shell
./venv/bin/mypy .

./venv/bin/pylint ./<some_file>.py
```

# Regression testing

The files `regression.{adb,ads}` try to set up some limited regression
testing. If you need to modify them, you can check that your changes are
sensible by running `gprbuild` in the current directory. This assumes that
you have gprbuild installed, which is the case if you use the nix shell.
