#!/bin/sh
cd "$(dirname "$0")"
set -eu

if ! command -v rack > /dev/null
then
    echo "rack cli tool not found in PATH"
    exit 1
fi


rack --base-url http://10.33.49.238 model import --clear ../../../RACK/RACK-Ontology/OwlModels/import.yaml
rack --base-url http://10.33.49.238 model import  ./import.yaml

rack --base-url http://10.33.49.238 data import --clear ../../../RACK/RACK-Ontology/OwlModels/DO-178C.yaml
rack --base-url http://10.33.49.238 data import ./Model.yaml

