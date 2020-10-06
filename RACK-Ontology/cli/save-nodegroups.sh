#!/bin/sh
# This script helps export changes to the nodegroups for
# storage in the repository
set -eu

if ! command -v rack > /dev/null
then
    echo "rack cli tool not found in PATH"
    exit 1
fi

rack nodegroups export "^ingest" ../../nodegroups/ingestion
rack nodegroups export "^query " ../../nodegroups/queries
