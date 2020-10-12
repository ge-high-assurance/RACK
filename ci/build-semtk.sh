#!/usr/bin/env bash
# Copyright (c) 2020, General Electric Company and Galois, Inc.

set -euo pipefail

sudo apt-get update
sudo apt-get install -y maven

rack_dir=${PWD}

pushd /tmp || exit 1

git clone \
    --quiet \
    --branch master \
    https://github.com/ge-semtk/semtk
cd semtk
git checkout "${SEMTK_COMMIT}"
mvn --quiet --batch-mode clean install -DskipTests
bash build.sh
cp "distribution/target/semtk-opensource-${SEMTK_VERSION}-bin.tar.gz" "${rack_dir}/packer/files/semtk-opensource-bin.tar.gz"

popd || exit 1
