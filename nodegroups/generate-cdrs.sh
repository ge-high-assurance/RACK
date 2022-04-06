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
rack model import RACK-Ontology/OwlModels/import.yaml
rack model import GE-Ontology/OwlModels/import.yaml
rack model import Boeing-Ontology/OwlModels/import.yaml
rack model import GrammaTech-Ontology/OwlModels/import.yaml
rack model import LM-Ontology/OwlModels/import.yaml
rack model import SRI-Ontology/OwlModels/import.yaml
rack model import STR-Ontology/OwlModels/import.yaml
rack model import RTX-Ontology/OwlModels/import.yaml

# Remove old CDR files

rm -rf nodegroups/CDR
rm -rf nodegroups/ingestion

# Generate new CDR files

java -cp "$STANDALONE_EXE_JAR" com.ge.research.semtk.standaloneExecutables.IngestTemplateGenerator --csv-dir nodegroups/CDR --json-dir nodegroups/ingestion
