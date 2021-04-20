# RACK migration tool

NOTE: eventually we can have the tool migrate between different named versions
of RACK revisions.  This current prototype solely works on updating nodegroups
from the v4.1 to the v5.0 revision.

Some changes may have been missed as they were manually listed. If you encounter
issues with some migration, feel free to reach out to Val (val@galois.com) for
help.

## How to build?

To set up, we suggest using Python's virtualenv.  A possible setup is to run:

```
python3 -m venv env       # Creates a virtual environment in the 'env' directory
source env/bin/activate   # Sets up the virtual environment in the **current** shell
```

In a shell where the environment is activated, you ought to be able to:

```
python3 -m pip install -r requirements.txt
# or, if you intend to modify the tool:
python3 -m pip install -r requirements.txt -r requirements-dev.txt
```

And finally build and set up the tool with:

```
python3 setup.py install
```

## How to run?

Once in an environment where the tool has been installed, the current invocation
is:

```
rack_migrate --from-folder <path> --to-folder <path>
```

and it will try and migrate all JSON files from the "from" folder, outputting to
the "to" folder.  We recommend against making these the same folder as the tool
is still in development.

You can pass an additional `--log-level=<LEVEL>` where `<LEVEL>` can be one
of `INFO` (default verbosity), or `DEBUG` for details about every step being
taken.
