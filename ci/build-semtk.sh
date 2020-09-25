#!/usr/bin/env bash

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
cp distribution/target/semtk-opensource-2.2.1-SNAPSHOT-bin.tar.gz "${rack_dir}/packer/files/"

popd || exit 1
