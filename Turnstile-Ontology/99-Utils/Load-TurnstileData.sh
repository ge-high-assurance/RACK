#!/bin/sh
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu
BASEDIR=$(dirname "$0")

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

# Turnstile ontology overlay
rack model import "$BASEDIR/import.yaml"

# nodegroups created specifically for Turnstile ingestion
rack nodegroups import "$BASEDIR/NodeGroups"

# Load applicable standards
rack data import --clear ../../RACK-Ontology/OwlModels/DO-178C.yaml   # from datagraph http://rack001/do-178c

# Turnstile instance data
rack data import --clear "$BASEDIR/Data/Model.yaml"                   # from datagraph http://rack001/data
