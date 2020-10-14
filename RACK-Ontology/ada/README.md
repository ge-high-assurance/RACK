# Setup

The project is set up for easy use with `nix` and `pipenv`.

Using nix, you just need to enter the shell (using `nix-shell`) to be in an
environment with python and libadalang installed. To double-check, run the
`python` REPL and type `import libadalang`, it should succeed.

We recommend using `direnv` to automatically load the shell environment for you
whenever you enter this directory.

You can run the executable, as well as development tools, via `pipenv`. For
instance, to type-check the entire project, you can:

```shell
pipenv run mypy .
```

To run the analysis on the regression test (NOTE: this is likely to change
shortly, so this may not be up to date):

```shell
pipenv run ./analyze-libadalang.py --project ./regression.gpr ./regression.adb
```
