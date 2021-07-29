#!/bin/sh
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu
BASEDIR=$(dirname "$0")
echo "$BASEDIR"

rack --base-url http://192.168.21.112 nodegroups import . 
rack --base-url http://192.168.21.112 data export --data-graph http://rack001/turnstiledata "query_forSystemSpec_turnstiledata" --file systemspec.csv --format csv 
