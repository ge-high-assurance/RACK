#!/bin/sh
set -eu

if ! command -v rack > /dev/null
then
    echo "rack cli tool not found in PATH"
    exit 1
fi

rack model import ../OwlModels/import.yaml
rack nodegroups import ../../nodegroups/ingestion
rack nodegroups import ../../nodegroups/queries
rack data import --clear ../models/TurnstileSystem/Data/import.yaml
