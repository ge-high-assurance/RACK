#!/bin/sh
# Copyright (c) 2021, General Electric Company and Galois, Inc.
set -eu
BASEDIR=$(dirname "$0")

# You need to pass in the standalone executables jar

STANDALONE_EXE_JAR=$(realpath "$1")

# Check rack cli tool is in PATH

cd "$BASEDIR"/..
cli/ensure-cli-in-PATH.sh

# Import ontologies

rack model clear
rack model import RACK-Ontology/OwlModels/model.yaml
rack model import overlays/GE-Ontology/OwlModels/model.yaml
rack model import overlays/Boeing-Ontology/OwlModels/model.yaml
rack model import overlays/GrammaTech-Ontology/OwlModels/model.yaml
rack model import overlays/LM-Ontology/OwlModels/model.yaml
rack model import overlays/SRI-Ontology/OwlModels/model.yaml
rack model import overlays/STR-Ontology/OwlModels/model.yaml
rack model import overlays/RTX-Ontology/OwlModels/model.yaml

# Remove old CDR files

rm -rf nodegroups/CDR
rm -rf nodegroups/ingestion

# Generate new CDR files

java -cp "$STANDALONE_EXE_JAR" com.ge.research.semtk.standaloneExecutables.IngestTemplateGenerator --csv-dir nodegroups/CDR --json-dir nodegroups/ingestion
