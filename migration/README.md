# RACK migration tool

The RACK migration tool allows one to automatically perform most of the updating
needed for nodegroups when the RACK ontology changes.  Most required changes are
entirely determined by the nature of the ontology change, and so the migration
tool can perform these automatically.  Some ontology changes, however, have
consequences on the nodegroups that are best resolved by an advised human
operator, and so the tool does not attempt to solve those.

Some changes may have been missed as they were manually listed. If you encounter
issues with some migration, feel free to reach out to Val (val@galois.com) for
help.

## How to build?

To set up, we suggest using Python's virtualenv.  A possible setup is to run
(replacing `env` with the name of the folder you want your virtual environment
saved in):

```
python3 -m venv env       # Creates a virtual environment in the 'env' directory
source env/bin/activate   # Sets up the virtual environment in the **current** shell
```

The rest of the procedure uses `pip`, which is added to your `PATH` by the
virtual environment activation command.  If you open a new shell, make sure to
run `source` again to reopen the virtual environment!

In a shell where the environment is activated, you ought to be able to:

```
pip install -r requirements.txt
# or, if you intend to work on the tool:
pip install -e -r requirements.txt -r requirements-dev.txt
```

And finally build and set up the tool with:

```
# still from the migration directory
pip install .
```

## How to run?

Once in an environment where the tool has been installed, the current invocation
is:

```
rack_migrate --from-folder <path> --to-folder <path> --old-ref <git_reference> --new-ref <git_reference>
```

and it will try and migrate all JSON files from the "from" folder, outputting to
the "to" folder.  We recommend against making these the same folder as the tool
is still in development.

You can pass an additional `--log-level=<LEVEL>` where `<LEVEL>` can be one
of `INFO` (default verbosity), or `DEBUG` for details about every step being
taken.

The mandatory `old-ref` and `new-ref` arguments allow the tool to know which
time frame you're looking to migrate between.  We recommend using tags like
`v4.0`, `v5.0`, `master`, but the command also accepts individual commits. To
ensure the tool will work, you should only use commits from the main development
branch.

You can also use the following command to list all known changes between two
revisions without migrating anything:

```
rack_migrate --list-changes-only --old-ref <git_reference> --new-ref <git_reference>
```

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
