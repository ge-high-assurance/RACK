# Nodegroups Folder

Each sub-folder has nodegroups for a particular purpose / application.

A **nodegroup** is a SemTK construct that represents a sub-graph.  SemTK generates SPARQL queries from nodegroups.  They also may contain ingestion templates which allow SemTK to map CSV files to the subgraph and generate INSERT queries.

### Sub-folders:
- **TA1-TurnstileIngest** - for ingesting the Turnstile example CSV files
- **TA3-Queries** - sample queries for TA3

```
NOTE: these nodegroups may already be available
      in the SPARQLgraph application under the menu
      Nodegroup->Load from store...
```