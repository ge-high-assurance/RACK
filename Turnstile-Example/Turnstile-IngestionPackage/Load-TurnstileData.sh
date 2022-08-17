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

echo "Updating File references..."
find "$BASEDIR" -name "*.csv" -exec sed -i -e "s|{{BASEDIR}}|$URLBASE|g" {} +

echo "Clear data-graph http://rack001/turnstiledata"
rack data clear --data-graph "http://rack001/turnstiledata"

echo "Ingesting Counter Application Unit Testing ..."
echo "(Have to do this one first because it ingests triples, as a demonstration. The rest is ingested via nodegroups.)"
rack data import "$BASEDIR"/CounterApplicationUnitTesting/OwlModels/import.yaml

echo "Ingesting Development Plan Data ..."
rack data import "$BASEDIR"/TurnstileDevelopmentPlanData/import.yaml

echo "Ingesting Planning Document Evidence ..."
rack data import "$BASEDIR"/PlanningDocuments/import.yaml

echo "Ingesting Hazard Assessment Evidence ..."
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

echo "Ingesting System Spec ..."
rack data import "$BASEDIR"/TurnstileSystemSpec/import.yaml

echo "Ingesting Counter Application Requirement Spec ..."
rack data import "$BASEDIR"/CounterApplicationRequirementSpec/import.yaml

echo "Ingesting Counter Application Software Design ..."
rack data import "$BASEDIR"/CounterApplicationSoftwareDes/import.yaml

echo "Ingesting System Verification Report ..."
rack data import "$BASEDIR"/SystemVerificationReport/import.yaml

echo "Ingesting Objectives ..."
rack data import "$BASEDIR"/Objectives/import.yaml

echo "Ingesting Baselines ..."
rack data import "$BASEDIR"/TurnstileBaselines/import.yaml

echo "----------------------------------------------------------------------"
echo "Static data ingestion completed.  Will now attempt to build the"
echo "Turnstile source code while using ASSIST to capture the dynamic"
echo "build information and upload the result to RACK.  This may fail"
echo "if the current environment does not support the software build"
echo "process (it will succeed if run via the RACK-in-a-Box process)."
echo
echo "Example:"
echo "  $ docker exec -w /home/ubuntu/RACK/Turnstile-Example/Turnstile-IngestionPackage CONTAINER  ./Load-TurnstileData.sh"
echo
echo "Errors after this point will result in partial (but still useable)"
echo "Turnstile sample data"
echo

(
    set -e
    cd "$BASEDIR"/CounterApplicationImplementation
    export PATH="${BASEDIR}/../../assist/bin:${BASEDIR}/../../assist/databin:${PATH}"
    make clean
    make
    make test
    make dist
    ingest_data -r "${BASEDIR}/turnstile-ingest.rack" -O "${BASEDIR}"/../../GE-Ontology http://rack001/turnstiledata .
)
