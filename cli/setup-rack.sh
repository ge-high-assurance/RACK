#!/bin/sh
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu

if ! command -v rack > /dev/null
then
    echo "rack cli tool not found in PATH"
    exit 1
fi

rack model import --clear ../RACK-Ontology/OwlModels/import.yaml
rack model import ../Turnstile-Ontology/99-Utils/import.yaml
rack model import ../GrammaTech-Ontology/import.yaml
rack model import ../STR-Ontology/import.yaml

rack nodegroups delete --yes --regexp --ignore-nonexistent "^ingest[0-9]+" "^query " "^Ingest-"
rack nodegroups import ../Turnstile-Ontology/99-Utils/NodeGroups
rack nodegroups import ../nodegroups/queries
rack nodegroups import ../nodegroups/ingestion

rack data import --clear ../RACK-Ontology/OwlModels/DO-178C.yaml
rack data import ../Turnstile-Ontology/99-Utils/Data/Model.yaml
