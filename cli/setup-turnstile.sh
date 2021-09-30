#!/bin/sh
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu

./ensure-cli-in-PATH.sh

./setup-rack.sh

rack model import ../GE-Ontology/OwlModels/import.yaml

rack nodegroups import ../nodegroups/ingestion/arcos.turnstile
../Turnstile-Example/Turnstile-IngestionPackage/Load-TurnstileData.sh
