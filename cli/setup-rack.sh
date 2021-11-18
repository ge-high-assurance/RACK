#!/bin/sh
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu

./ensure-cli-in-PATH.sh

./setup-clear.sh

# RACK core ontology
rack model import ../RACK-Ontology/OwlModels/import.yaml

# ingestion nodegroups auto-generated from RACK core ontology, and a set of sample query nodegroups
rack nodegroups import ../nodegroups/ingestion/arcos.rack
rack nodegroups import ../nodegroups/queries
