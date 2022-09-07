#!/bin/bash
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu
BASEDIR=$(cd "$(dirname "$0")"; pwd)
echo "$BASEDIR"
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

# suppress RACK cli warnings about missing columns
export LOG_LEVEL=ERROR

if test "$OSTYPE" == "cygwin" -o "$OSTYPE" == "msys"; then
URLBASE=$(cygpath -m "$BASEDIR")
else
URLBASE="$BASEDIR"
fi


echo "Ingesting Resolution Data ..."
rack data import "$BASEDIR"/Resolutions-1/import.yaml

