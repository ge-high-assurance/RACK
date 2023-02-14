#!/bin/sh
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu

./ensure-cli-in-PATH.sh

rack --log-level ERROR manifest import  --clear ../manifests/arcos.yaml
