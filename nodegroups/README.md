# Nodegroups Folder

Each sub-folder has nodegroups for a particular purpose / application.

A **nodegroup** is a SemTK construct that represents a sub-graph.  SemTK generates SPARQL queries from nodegroups.  They also may contain ingestion templates which allow SemTK to map CSV files to the subgraph and generate INSERT queries.

## Sub-folders:
- **ingestion** - For populating RACK via CSV files
- **queries** - Sample derived RACK queries

```
NOTE: these nodegroups may already be available
      in the SPARQLgraph application under the menu
      Nodegroup->Load from store...
```

## Manually populating SemTK with a query set

Loading queries sets is documented on
[semtk/Standalone Executables](https://github.com/ge-semtk/semtk/wiki/Standalone-Executables)

```
java -cp /path/to/standaloneExecutables-jar-with-dependencies.jar com.ge.research.semtk.standaloneExecutables.StoreNodeGroup http://server:12056 store_data.csv
```
