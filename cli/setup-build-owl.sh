#!/bin/sh

set -e

REPO_DIR=$(dirname "$PWD")

docker pull sadl/sadl-eclipse:v3.5.0-20211130
docker run --rm -v "$REPO_DIR:/RACK" sadl/sadl-eclipse:v3.5.0-20211130 -importAll /RACK -cleanBuild
