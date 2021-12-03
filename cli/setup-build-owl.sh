#!/bin/sh
# Copyright (c) 2021, General Electric Company and Galois, Inc.

set -e

RACK_DIR=$(realpath $(dirname "$0")/..)

docker run --rm -v "$RACK_DIR:/RACK" sadl/sadl-eclipse:v3.5.0-20211130 -importAll /RACK -cleanBuild
