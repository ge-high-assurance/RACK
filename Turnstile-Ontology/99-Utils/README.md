# 99-Utils

This directory contains utilities related to ingesting Turnstile Data into RACK. The Turnstile example is setup to automatically generate CDR CSV files from node groups based on the SADL data in this project.

## How to create new CDR File

Creating a new CDR starts with generating a node group using SemTK. No import mapping is required, but care should be given to assigning parameter/connections as optional or not. Once the node group is constructed as desired download the node group from SemTK and place into /Turnstile-Ontology/99-Utils/NodeGroups folder. Rename the node group json using the following convention "Ingest-{Description}.json".

Once the new node group is saved in the folder perform a clean build of the eclipse project. The eclipse project is set up to run CreateIngestionData.py before the build process (this script can be ran manually as well). This script automates the following:

1. Updates the node group json file to add import mapping to the template CSV. Also adjusts the SparqlIDs to be more meaningfull names for the properties. New SparqlID follows the pattern {propType}_{nodeName}.  This fixes the issue of having "identifier", "identifier_0", "identifier_1", etc. The ingestion also replaces "null" with "". This may run into issues if a field has a string that includes "null". Currently this is not an issue.

2. Creates a CDR template CSV file located /Turnstile-Ontology/99-Utils/CsvTemplates. Template is a CSV file with headers that corespond to the import mapping for the updated node group.

3. Drafts a CDR spec located in /Turnstile-Ontology/99-Utils/CsvSpecs, The Spec documents the nodes, properties, and connections in the node group. As well documenting the primary key columns and option columns for the CSV template. Right now Node/Property notes are not inlcuded in the spec but that may be provided in the future.

4. Generates node group CLI Configuration CSV located in /Turnstile-Ontology/99-Utils/NodeGroups/. This will load all the node groups into Rack when setup-rack.sh is ran.

5. Generates DataQueries.sadl located in /Turnstile-Ontology/99-Utils/. This SADL file when ran in  eclipse (SADL->Test Model) Generates CDR CSV files in /Turnstile-Ontology/99-Utils/Data/ for each of the node groups using the CDR template.

6. Generates  CLI configuration Model.yaml located in /Turnstile-Ontology/99-Utils/Data/. This will ingest each of the CDR CSV generated into Rack when setup-rack.sh is ran.
