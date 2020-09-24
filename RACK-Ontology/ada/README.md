# How to build libadalang

- Enter the provided shell via `nix-shell`.  It will build lots of Ada libraries
  and the gprbuild tool suite.

- Run the provided `setup.sh` (at the moment, it can be done within or outside
  the shell).  It will clone the `langkit` repository in the `libadalang`
  submodule (since we cannot checkout a submodule within another).

- Enter the `libadalang` directory, and create a Python virtual environment,
  source it, and install the Python requirements, by doing, say:

  ```
  python -mvenv .env
  source ./.env/bin/activate
  pip install -r REQUIREMENTS.dev
  ```

- Additionally, pretty-printing Python code seems to require the Python library
  black, which is not listed in REQUIREMENTS.dev, so install it manually:

  ```
  pip install black
  ```

- In the `libadalang` directory, with the virtual environment, you can generate
  the bindings using:

  ```
  python ada/manage.py generate
  ```

At the moment, you should get errors about pretty-printing, I'm not sure they
matter yet and will try to solve them soon.

- In the `libadalang` directory, with the virtual environment, you can build the
  library using:

  ```
  python ada/manage.py --library-types=static,static-pic,relocatable build --build-mode prod
  ```

- You can install it via:

  ```
  python ada/manage.py --library-types=static,static-pic,relocatable install --build-mode prod out
  ```
