#!/bin/sh
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu

if ! command -v rack > /dev/null
then
    echo "rack cli tool not found in PATH"
    exit 1
fi

rack model import --clear ../RACK-Ontology/OwlModels/import.yaml
rack nodegroups delete --yes --regexp --ignore-nonexistent "^ingest[0-9]+" "^query "
rack nodegroups import ../nodegroups/ingestion
rack nodegroups import ../nodegroups/queries
rack data import --clear ../RACK-Ontology/models/TurnstileSystem/Data/import.yaml
rack data import ../RACK-Ontology/OwlModels/requirements.yaml
