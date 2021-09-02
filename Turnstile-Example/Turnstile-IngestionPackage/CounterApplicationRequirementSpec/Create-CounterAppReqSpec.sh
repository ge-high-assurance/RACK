#!/bin/sh
# Copyright (c) 2021, General Electric Company and Galois, Inc.
set -eu
BASEDIR=$(dirname "$0")
echo "$BASEDIR"

rack nodegroups import . 
rack data export --data-graph http://rack001/turnstiledata "query_forCounterAppReqSpec_turnstiledata" --file counterappreqspec.csv --format csv 
