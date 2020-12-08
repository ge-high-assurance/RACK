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

rack data import --clear $BASEDIR/Model.yaml
