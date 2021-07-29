#!/bin/sh
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu
BASEDIR=$(dirname "$0")
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

echo "First, load the GE-Ontology and nodegroups"
rack model import ../../GE-Ontology/OwlModels/import.yaml
rack nodegroups import ../../nodegroups/ingestion/arcos.turnstile

echo "Then, clear data-graph http://rack001/turnstiledata"
rack data clear --data-graph "http://rack001/turnstiledata"

echo "Ingesting Development Plan Data ..."
rack data import "$BASEDIR"/TurnstileDevelopmentPlanData/import.yaml

echo "Ingesting Planning Document Evidence ..."
rack data import "$BASEDIR"/PlanningDocuments/import.yaml

echo "Ingesting Hazard Assesment Evidence ..."
rack data import "$BASEDIR"/HazardAssessment/import.yaml

echo "Ingesting System Design Evidence ..."
rack data import "$BASEDIR"/TurnstileSystemDesign/import.yaml

echo "Ingesting Requirements ..."
rack data import "$BASEDIR"/TurnstileSystemRequirements/import.yaml
rack data import "$BASEDIR"/TurnstileHighLevelRequirements/import.yaml
rack data import "$BASEDIR"/TurnstileLowLevelRequirements/import.yaml
echo "Ingesting Requirement Model..."
rack data import "$BASEDIR"/TurnstileRequirementModel/import.yaml

echo "Ingesting Counter Application Review ..."
rack data import "$BASEDIR"/CounterApplicationReviews/import.yaml

echo "Ingesting Counter Application Testing ..."
rack data import "$BASEDIR"/CounterApplicationTesting/import.yaml

echo "Ingesting Counter Application Unit Testing ..."
rack data import "$BASEDIR"/CounterApplicationUnitTesting/OwlModels/import.yaml
