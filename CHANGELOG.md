# RACK Change Log

## [v5.0] - 2021-02-27

- Made some changes to RACK ontology (click
  [here](https://github.com/ge-high-assurance/RACK/wiki/RACK-v5.0-Ontology-Changelog)
  for more details)
- Updated ASSIST tooling with ability to use additional recognizers
- Updated Turnstile example to use ASSIST for software structure data
  generation and ingestion
- Pulled process data into extra data graphs with separate names
  (ARP-475A, DO-330, DO-178C, MIL-STD-881D)
- Updated names of query nodegroups' returned results to follow naming
  convention (click [here](https://github.com/ge-high-assurance/RACK/blob/master/nodegroups/queries/README.md) for more details)
- Added example to Turnstile illustrating how to provide dateTime
  information in a CSV file (click [here](https://github.com/ge-high-assurance/RACK/wiki/RACK-dateTime) for more details)
- Added Ada analysis tooling from Galois

## [v4.1] - 2020-12-17

- Added STR ontology overlays
- Added a set of generic ingestion nodegroups and CDRs
- Created finer wasDerivedFrom distinctions in RACK ontology

## [v4.0] - 2020-12-07

- Moved a few directories out of RACK-Ontology to their own places
- Added SPARQLgraph union queries
- Added GrammaTech ontology overlays
- Added Provenance-Example illustrating how to record who, when, and
  how data is entered into RACK
- Updated Turnstile sample data with examples of satisfiedBy DO-178C
  objectives (see Objective.csv and the Objective ingest nodegroup)
- Automated ingestion for Turnstile data (node groups and SADL files
  used to generate CSV files, in turn used to run queries, in turn
  used to generate CSV files for data ingestion)
