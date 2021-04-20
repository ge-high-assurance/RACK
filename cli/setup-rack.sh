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

rack model import --clear ../RACK-Ontology/OwlModels/import.yaml
rack model import ../Turnstile-Ontology/99-Utils/import.yaml
rack model import ../GrammaTech-Ontology/import.yaml
rack model import ../STR-Ontology/import.yaml

rack nodegroups delete --yes --regexp --ignore-nonexistent "^ingest" "^query " "^Ingest-"
rack nodegroups import ../Turnstile-Ontology/99-Utils/NodeGroups
rack nodegroups import ../nodegroups/ingestion
rack nodegroups import ../nodegroups/queries

rack data import --clear ../RACK-Ontology/OwlModels/ARP-4754A.yaml
rack data import --clear ../RACK-Ontology/OwlModels/DO-330.yaml
rack data import --clear ../RACK-Ontology/OwlModels/DO-178C.yaml
rack data import --clear ../RACK-Ontology/OwlModels/MIL-STD-881D.yaml
rack data import --clear ../Turnstile-Ontology/99-Utils/Data/Model.yaml
