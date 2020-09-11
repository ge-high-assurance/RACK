This directory contains Prolog modules offering tools for loading OWL models in
and out of a Fuseki instance, as well as several Prolog relations for querying
the ontology.

You can read the code's documentation, as well as a rendering of this very file,
by running `swipl` **from this directory**, and entering:

```prolog
[documentation/serve].
```

This command should set up a local pldoc webserver instance listening at
[http://localhost:4040/pldoc](http://localhost:4040/pldoc).

# Command-line Usage

NOTE: For the following commands to work, you will want to have generated some
data for them to use.  For instance, to get the "TurnstileSystem" data
generated, you can run the `Makefile` in
`RACK-Ontology/models/TurnstileSystem/src` while prepending
`RACK-Ontology/databin` to your `PATH`.  This will ensure that the script uses
the instrumented `gcc` that records data.

There are a number of command-line utilities that can be used with the
Prolog modules in this directory.

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

# Prolog API Usage

## Setup

In order to load `rack_model.pl`, you will need a Prolog interpreter.  One such
interpreter is the SWI Prolog implementation.  You can use it to load the module
in a read-eval print loop (REPL) by doing:

```shell
swipl -s rack_model.pl
```

## Manipulating models

### Loading

You may load a model in memory using one of the three following forms:

```prolog
load_local_model('../ontology/OwlModels').
load_model_from_url('http://192.168.0.32:3030/RACK/').
load_model_from_rack().
```

The local form takes as argument a path to a directory containing OWL files to load.

The URL form expects to find a Fuseki instance.

The Rack form expects to find a Fuseki instance living at `localhost:3030/RACK`,
which can easily be obtained for testing by running:

```shell
fuseki-server --mem RACK/
```

### Saving locally

There are two forms for saving the model in memory to a local file:

```prolog
save_model_to_file('path/to/filename.owl').
save_model_to_file('path/to/filename.owl', 'SomeNamespace').
```

They will both write out to the specified file, the latter allowing the
specification of a namespace for the triples.

### Uploading to Fuseki

There are two forms for uploading the model in memory to a Fuseki instance:

```prolog
upload_model_to_url('http://192.168.0.32:3030/RACK').
upload_model_to_rack().
```

As for loading, the `upload_model_to_url/1` expects to find a Fuseki instance at
that location, while `upload_model_to_rack/0` expects the instance to live at
`localhost:3030/RACK`.
