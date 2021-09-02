#!/bin/sh
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu

if ! command -v rack > /dev/null
then
	cat <<-END
		ERROR: rack cli tool not found in PATH
		
		Installation instructions are available at
		https://github.com/ge-high-assurance/RACK/wiki/RACK-CLI#install-dependencies
		or locally in README.md
		
		If you've already installed RACK CLI, please activate your virtual environment
		
		macOS/Linux: source venv/bin/activate
		Windows:     venv\\Scripts\\activate.bat
		PowerShell:  venv\\Scripts\\Activate.ps1
	END
	exit 1
fi

./setup-clear.sh

# RACK core ontology
rack model import ../RACK-Ontology/OwlModels/import.yaml

# ingestion nodegroups auto-generated from RACK core ontology, and a set of sample query nodegroups 
rack nodegroups delete --yes --regexp --ignore-nonexistent "^ingest" "^query"
rack nodegroups import ../nodegroups/ingestion/arcos.rack
rack nodegroups import ../nodegroups/queries
