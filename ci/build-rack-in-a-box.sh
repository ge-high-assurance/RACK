#!/usr/bin/env bash

set -euo pipefail

tar cf /tmp/rack.tar.gz --exclude=.git --exclude=packer .
mv /tmp/rack.tar.gz packer/files/

cd packer

function assert_exists() {
  if ! [[ -f "files/${1}" ]]; then
    echo "Error in CI configuration: Expected files/${1} to exist!"
    ls files/
    exit 1
  fi
}
assert_exists "semtk-opensource-bin.tar.gz"

fuseki_tarball="files/apache-jena-fuseki-${FUSEKI_VERSION}.tar.gz"

if ! [[ -f "${fuseki_tarball}" ]]; then
  curl \
    --location \
    --silent \
    --show-error \
    --output "${fuseki_tarball}" \
    "https://mirrors.gigenet.com/apache/jena/binaries/apache-jena-fuseki-${FUSEKI_VERSION}.tar.gz"
fi

systemctl_script="files/systemctl3.py"
if ! [[ -f "${systemctl_script}" ]]; then
  curl \
    --location \
    --silent \
    --show-error \
    --output "${systemctl_script}" \
    "https://raw.githubusercontent.com/gdraheim/docker-systemctl-replacement/master/files/docker/systemctl3.py"
fi

touch files/{documentation.html,index.html,style.css}

packer build rack-box-docker.json
docker image ls
