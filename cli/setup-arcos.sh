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

rack model import ../GrammaTech-Ontology/import.yaml
rack model import ../LM-Ontology/import.yaml
# rack model import ../DesCert-Ontology/import.yaml
rack model import ../STR-Ontology/import.yaml
rack model import ../Boeing-Ontology/import.yaml

# rack data import --clear ../RACK-Ontology/OwlModels/ARP-4754A.yaml     # from datagraph http://rack001/arp-475a
# rack data import --clear ../RACK-Ontology/OwlModels/DO-330.yaml        # from datagraph http://rack001/do-330
rack data import --clear ../RACK-Ontology/OwlModels/DO-178C.yaml         # from datagraph http://rack001/do-178c
# rack data import --clear ../RACK-Ontology/OwlModels/MIL-STD-881D.yaml  # from datagraph http://rack001/mil-std-881d