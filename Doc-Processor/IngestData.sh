#!/bin/bash
BASEDIR=$(dirname "$0")
echo "$BASEDIR"

cd $BASEDIR/../cli
source venv/bin/activate

set -eu

if ! command -v rack > /dev/null
then
    echo "rack cli tool not found in PATH"
    exit 1
fi
pwd
rack data import --clear ../Doc-Processor/Model.yaml
