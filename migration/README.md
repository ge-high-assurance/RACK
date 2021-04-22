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
# from the migration directory
pip install .
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

# RACK crawler

This tool is meant for internal use by the developers of the RACK migration tool
and should not be useful to users of the tool.

The RACK crawler takes as input two revisions, and currently MUST BE RUN from
the `migration` folder, in a folder that is a git repository of RACK.  The
crawler will look through all commits between the two given revisions, notice
when they modify ontology files, and check whether the `migration/rack/commits/`
directory contains a corresponding file.

If the file is missing, the tool will copy a template there, and add it to the
list of commits in `migration/rack/commits/__init__.py`.  After this, there are
still two manual steps needed from the human.

First, for each created commit file, they must go in the generated file, and
formalize the changes made by the commit.

Second, they must go in `migration/rack/commits/__init__.py`, and make sure to
order the commits in the correct anti-chronological order.  This will ensure
that the RACK migration tool process those changes in the correct order.

Usage:

```
rack_crawl --old-ref <git-ref> --new-ref <git-ref>
```

where `git-ref` may be either a commit ID or a tag, anything git accepts as a
reference.  `--new-ref` is optional and defaults to `master`.
