#!/bin/sh
# Copyright (c) 2020, General Electric Company and Galois, Inc.
#
# This script helps export changes to the nodegroups for
# storage in the repository
set -eu

if ! command -v rack > /dev/null
then
    echo "rack cli tool not found in PATH"
    exit 1
fi

rack nodegroups export "^query " ../RACK-Ontology/nodegroups
