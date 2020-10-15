# Nodegroups Folder

Each sub-folder has nodegroups for a particular purpose / application.

A **nodegroup** is a SemTK construct that represents a sub-graph.
SemTK generates SPARQL queries from nodegroups.  They also may contain
ingestion templates which allow SemTK to map CSV files to the subgraph
and generate INSERT queries.

## Sub-folders:

- **ingestion** - For populating RACK via CSV files
- **queries** - Sample derived RACK queries

NOTE: these nodegroups may already be available in the SPARQLgraph
application under the menu `Nodegroup->Load from store...`.

## Manually populating SemTK with a query set

Loading queries sets is documented on [semtk/Standalone
Executables](https://github.com/ge-semtk/semtk/wiki/Standalone-Executables):

```shell
java -cp /path/to/standaloneExecutables-jar-with-dependencies.jar com.ge.research.semtk.standaloneExecutables.StoreNodeGroup http://server:12056 store_data.csv
```

NOTE: Check out the RACK [command-line
interface](../RACK-Ontology/cli/) which may be easier to use.

---
Copyright (c) 2020, General Electric Company, Galois, Inc.

All Rights Reserved

This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.

Any opinions, findings and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the Defense Advanced Research Projects Agency (DARPA).
