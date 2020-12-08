#!/bin/bash
cd /home/ubuntu/RACK/cli
source venv/bin/activate
set -eu

if ! command -v rack > /dev/null
then
    echo "rack cli tool not found in PATH"
    exit 1
fi

rack model import ../RACK-Ontology/OwlModels/import.yaml

rack nodegroups import ../Doc-Processor/nodegroups


