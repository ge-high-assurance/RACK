This directory contains Prolog modules offering tools for loading OWL models in
and out of a Fuseki instance, as well as several Prolog relations for querying
the ontology.

# Command-line Usage

NOTE: For the following commands to work, you will want to have
generated some data for them to use.  For instance, to get the
"TurnstileSystem" data generated, you can run `make` on the `Makefile`
in `Turnstile-Ontology/02-Software/03-Implementation` while prepending
`RACK-Ontology/databin` to your `PATH`.  This will ensure that the
script uses the instrumented `gcc` that records data.

There are a number of command-line utilities that can be used with the
Prolog modules in this directory:

 * `analyze` Runs an analysis of the ontology model and any associated
   instantiated data.  Command-line options can be used to specify the
   data model to be loaded and which subset of the data model
   instances to display.

   ```shell
   $ analyze -h
   $ analyze -i http://turnstilesystem/CounterApplication
   ```

 * `check` Runs various tests and checks on the ontology model and any
   associated instantiated data.  Similar command-line options to the
   `analyze` command, viewable via the `-h` flag.

   ```shell
   $ check -h
   $ check -v
   ```

   The various checks performed by this tool are designed to be
   extensible.  Adding new checks for ontology invariants is
   encouraged by both RACK developers and users.

 * `ingest_data` Loads the ontology model (from disk or RACK/Fuseki),
   a set of data recognizers, and then the data to be recognized,
   instantiating that data against the ontology model.  Writes the
   result to disk or back to the RACK/Fuseki server.  See the `-h`
   flag for usage details.

   ```shell
   $ ingest_data -h
   $ ingest_data http://TurnstileSystem/CounterApplication \
       models/TurnstileSystem/src
   ```

   The above would ingest all build/test datafiles generated during
   the build process in the `models/TurnstileSystem/src` directory.
   The results would be loaded into the RACK database being served at
   `http://localhost:3030` as instance data in the
   `http://TurnstileSystem/CounterApplication` namespace.

   The typical usage is to:

    1. perform a build with the `databin` directory
       in the `PATH` so that the wrappers in that directory can capture the
       build information into datafiles in that directory, then
    2. `ingest_data` to load that data as instances in the RACK database, then
    3. `analyze` or `check` that data.

  The `ingest_data` tool is designed to be extensible.  A RACK user
  can emit `.rack` files (or an equivalent) during their analysis of
  the data.  By creating corresponding data recognizers for the data
  in those `.rack` files, the `ingest_data` tool will be able to
  upload the recognized data into the RACK database for those
  processes as well as the ones defined in the RACK distribution.  For
  more information, see the documentation on "Recognizers for Loaded
  Data" in the [RACK model management Prolog file: rack/model.pl](rack/model.pl).

 * `sadl2owl` Performs a conversion of the `.sadl` files in an input
   directory (defaulting to `RACK-Ontology/ontology`) into `.owl`
   files in the specified output directory.  Command-line options are
   visible when invoking with no arguments or with the `-h` flag.

   ```shell
   $ sadl2owl RACK-OntologyOwlModels
   ```

   Note that this tool is a subset of the Eclipse SADL functionality.
   Primary development should still utilize Eclipse SADL.  This tool
   is intended for batch automation like CI processing and has the
   following limitations:

      - provides conversion for only a subset of SADL (the subset
        utilized by the current ontology specification, which should
        be extended as needed)

      - requires well-formed SADL as input (i.e. it does not provide
        helpful diagnostic or error messages).

      - Does not support the SADL active operators (e.g. `Ask`,
        `Rule`, or `Test`).

# Prolog API Usage

You can read the code's documentation, as well as a rendering of this very file,
by running `swipl` **from this directory**, and entering:

```prolog
[documentation/serve].
```

This command should set up a local pldoc webserver instance listening at
[http://localhost:4040/pldoc](http://localhost:4040/pldoc).

## Setup

In order to load `rack_model.pl`, you will need a Prolog interpreter.  One such
interpreter is the SWI Prolog implementation.  You can use it to load the module
in a read-eval print loop (REPL) by running, from the `RACK/` folder:

```shell
swipl -s ./assist/bin/rack/model.pl
```

## Manipulating models

### Loading

You may load a model in memory using one of the three following forms:

```prolog
load_local_model('./Turnstile-Ontology/OwlModels').
```

The local form takes as argument a path to a directory containing OWL files
to load.

NOTE: The path in this example may not correspond to the path on your
machine, please adapt the command accordingly.

---

```prolog
load_model_from_url('http://192.168.0.32:3030/').
```

This form expects to find a Fuseki instance.
NOTE: currently you should make sure to include the final '/'.

---

```prolog
load_model_from_rack().
```

This form expects to find a Fuseki instance living at `localhost:3030/RACK`,
which can easily be obtained for testing by running:

```shell
fuseki-server --mem RACK/
```

### Saving locally

There are two forms for saving the model in memory to a local file:

```prolog
save_model_to_file('path/to/filename.owl').
```

---

```prolog
save_model_to_file('path/to/filename.owl', 'SomeNamespace').
```

They will both write out to the specified file, the latter allowing the
specification of a namespace for the triples.

### Uploading to Fuseki

There are two forms for uploading the model in memory to a Fuseki instance:

```prolog
upload_model_to_url('http://192.168.0.32:3030/RACK').
```

---

```prolog
upload_model_to_rack().
```

Like in the loading case, the `upload_model_to_url/1` expects to find a
Fuseki instance at that location, while `upload_model_to_rack/0` expects the
instance to live at `localhost:3030/RACK`.

# Design Notes

## Node identifiers

  `PROV-S#ACTIVITY` operations are typically annotated by a date + PID
  (process ID) nonce value; repeated runs of the same activity will
  have a different nonce.

  `SOFTWARE#FILE` instances are typically annotated by a SHA1 hash of the file
  itself, for stable references that change only when the file itself
  changes (independent of filename).

  Build tools that invoke other build tools should have a way to
  correlate these invocations.  For the databin `make` wrapper, the
  wrapper sets the `MAKE_DATABIN` which other databin wrappers can use
  to associate themselves with.  The `MAKE_DATABIN` wrapper is a
  colon-separated list to handle recursive `make` invocations.  Other
  build tools can use a similar technique.

## Performance

The following data is collected for processing a moderately large code
base (turnstile + ffmpeg) and a very large codebase.

 | Codebase         | Tool    | Runtime | Notes                                              |
 |------------------|---------|---------|----------------------------------------------------|
 | moderately large | ingest  | 9m30s   | Saved 38,880 triples about 6,649 subjects          |
 |                  | -       |         |                                                    |
 |                  | analyze | 12.2s   | 1,878 http://arcos.rack/SOFTWARE#COMPILE instances |
 |                  |         |         | 4,756 http://arcos.rack/SOFTWARE#FILE instances    |
 |                  |         |         | 9     http://arcos.rack/PROV-S#ACTIVITY instances  |
 |------------------|---------|---------|----------------------------------------------------|
 | huge             | ingest  | 14m7s   | Saved 455,231 triples about 89,945 subjects        |
 |                  | -       |         |                                                    |
 |                  | analyze | 15m36s  | 1,880 http://arcos.rack/SOFTWARE#COMPILE instances |
 |                  |         |         | 88,049 http://arcos.rack/SOFTWARE#FILE instances   |

---
Copyright (c) 2020, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).

Distribution Statement "A" (Approved for Public Release, Distribution Unlimited)
