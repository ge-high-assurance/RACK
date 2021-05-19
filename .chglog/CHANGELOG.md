# RACK Change Log

## [v6.0] - 2021-05-03

This release has the following changes:

- Added LMCO & SRI ontology overlays
- Loaded only DO-178C in RiB, made ARP-4754A, DO-330, & MIL-STD-881D optional
- Added Boeing data validation query nodegreoups
- Added [dataInsertedBy](https://github.com/ge-high-assurance/RACK/wiki/How-to-use-dataInsertedBy) property and predefined nodegroup
- Improved construct query UI and functionality
- Added [RACK migration tool](https://github.com/ge-high-assurance/RACK/tree/master/migration)
- Automatically generated Wiki page for [RACK ontology changes](https://github.com/ge-high-assurance/RACK/wiki/RACK-ontology-detailed-changelogs)
- Automatically generated Wiki page for [RACK predefined queries](https://github.com/ge-high-assurance/RACK/wiki/RACK-Predefined-Queries)
- Streamlined Wiki (custom sidebar)

## [v5.0] - 2021-03-08

This release has the following changes:

- Made some changes to RACK ontology (click [here](https://github.com/ge-high-assurance/RACK/wiki/RACK-v5.0-Ontology-Changelog) for more details)
- Updated ASSIST tooling with ability to use additional recognizers
- Updated Turnstile example to use ASSIST for software structure data generation and ingestion
- Pulled process data into extra data graphs with separate names (ARP-475A, DO-330, DO-178C, MIL-STD-881D)
- Updated names of query nodegroups' returned results to follow naming convention (click [here](https://github.com/ge-high-assurance/RACK/blob/master/nodegroups/queries/README.md) for more details)
- Added example to Turnstile illustrating how to provide dateTime information in a CSV file (click [here](https://github.com/ge-high-assurance/RACK/wiki/RACK-dateTime) for more details)
- Added Ada analysis tooling from Galois
- Improved performance for data ingestion and querying
- Scripts and data for loading in Boeing example: RACK V5 is not preloaded with this data but these have been sent to all ARCOS performers.
- Added support for runtime constraints and multiple data-graphs to the RACK CLI (click [here](https://github.com/ge-high-assurance/RACK/wiki/RACK-CLI) for more details)

## [v4.1] - 2020-12-17

This release has the following changes:

- Added [STR ontology](https://github.com/ge-high-assurance/RACK/tree/master/STR-Ontology) overlays
- Added a set of generic [ingestion nodegroups](https://github.com/ge-high-assurance/RACK/tree/master/nodegroups/ingestion) and [CDRs](https://github.com/ge-high-assurance/RACK/tree/master/nodegroups/CDR)
- Created finer wasDerivedFrom distinctions in RACK ontology

## [v4.0] - 2020-12-07

This release has the following changes:

- Moved a few directories out of RACK-Ontology to their own places
- Added [GrammaTech ontology](https://github.com/ge-high-assurance/RACK/tree/master/GrammaTech-Ontology) overlays
- Added SPARQLgraph [union queries](https://github.com/ge-semtk/semtk/wiki/Queries-Advanced-Topics)
- Added [Provenance-Example](https://github.com/ge-high-assurance/RACK/tree/master/Provenance-Example) illustrating how to record who, when, and how data is entered into RACK
- Updated Turnstile sample data with examples of `satisfiedBy` DO-178C objectives (see [Objective.csv](https://github.com/ge-high-assurance/RACK/blob/master/Turnstile-Ontology/99-Utils/Data/Objective.csv) and the [Objective ingest nodegroup](https://github.com/ge-high-assurance/RACK/blob/master/Turnstile-Ontology/99-Utils/NodeGroups/Ingest-Objective.json))
- Automated ingestion for Turnstile data (node groups and SADL files used to generate CSV files, in turn used to run queries, in turn used to generate CSV files for data ingestion)
