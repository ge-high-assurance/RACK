#!/bin/sh
# Copyright (c) 2021, General Electric Company and Galois, Inc.
set -eu
BASEDIR=$(dirname "$0")
echo "$BASEDIR"

rack nodegroups import . 
rack data export --data-graph http://rack001/turnstiledata "query_forCounterAppSWDes_turnstiledata" --file counterappswdes.csv --format csv 
rack data import --data-graph http://rack001/turnstiledata import.yaml
