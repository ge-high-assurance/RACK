#!/bin/sh
# Copyright (c) 2021, General Electric Company and Galois, Inc.
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

rack model clear

rack data clear                             \
  --data-graph http://rack001/data          \
  --data-graph http://rack001/arp-475a      \
  --data-graph http://rack001/do-330        \
  --data-graph http://rack001/do-178c       \
  --data-graph http://rack001/mil-std-881d

rack nodegroups delete-all --yes
