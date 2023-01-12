#!/usr/bin/env bash
# Copyright (c) 2022, General Electric Company and Galois, Inc.

set -eu

echo "Stopping Fuseki"
systemctl stop fuseki

echo "Restarting Fuseki"
systemctl start fuseki
