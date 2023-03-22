#!/bin/sh
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu

./ensure-cli-in-PATH.sh

TMP=$(mktemp -d -t ingestion_package) || exit 1
trap 'rm -rf "$TMP"; trap - EXIT; exit' EXIT INT HUP

rack manifest build ../manifests/arcos.yaml "${TMP}/output"
rack manifest import --clear "${TMP}/output.zip"
