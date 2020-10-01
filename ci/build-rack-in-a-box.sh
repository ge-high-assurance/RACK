#!/usr/bin/env bash

set -euo pipefail

tar cf packer/files/rack.tar.gz --exclude=.git --exclude=packer .

cd packer

function assert_exists() {
  if ! [[ -f "files/${1}" ]]; then
    echo "Error in CI configuration: Expected files/${1} to exist!"
    ls files/
    exit 1
  fi
}
assert_exists "semtk-opensource-bin.tar.gz"

function get() {
  if ! [[ -f "${1}" ]]; then
  curl \
    --location \
    --silent \
    --show-error \
    --output "${1}" \
    "${2}"
  fi
}

get \
  "files/apache-jena-fuseki-${FUSEKI_VERSION}.tar.gz" \
  "https://mirrors.gigenet.com/apache/jena/binaries/apache-jena-fuseki-${FUSEKI_VERSION}.tar.gz"

get \
  "files/systemctl3.py" \
  "https://raw.githubusercontent.com/gdraheim/docker-systemctl-replacement/master/files/docker/systemctl3.py"

get \
  "files/style.css" \
  "https://raw.githubusercontent.com/KrauseFx/markdown-to-html-github-style/master/style.css"

touch files/index.html

packer build rack-box-docker.json
docker image ls
