#!/bin/sh
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu

./ensure-cli-in-PATH.sh

rack manifest import --clear ../manifests/turnstile.yaml
