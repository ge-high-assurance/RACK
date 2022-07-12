#!/bin/bash
# Copyright (c) 2020, General Electric Company and Galois, Inc.

BASEDIR=$(cd "$(dirname "$0")" || exit; pwd)
echo "Installing RACK-STK from $BASEDIR"

echo "Creating Derived files from Local RACK instance"

python3 "$BASEDIR"/AutoGeneration/Generate-STK.py

pip3 install "$BASEDIR" --upgrade
