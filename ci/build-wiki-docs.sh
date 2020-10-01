#!/usr/bin/env bash

set -euo pipefail

cd ./RACK.wiki
sudo npm install -g github-wikito-converter
gwtc -t RACK-in-a-Box .
mv documentation.html ../RACK/packer/files
