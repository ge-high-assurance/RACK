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

installed() { command -v "$1" >/dev/null 2>&1; }

# Install Packer if it's absent (i.e. when using `act`)
if ! installed packer; then
  pushd /tmp || exit 1

  ver="1.6.2"
  curl \
    --location \
    --silent \
    --show-error \
    --output "packer_${ver}_linux_amd64.zip" \
    "https://releases.hashicorp.com/packer/${ver}/packer_${ver}_linux_amd64.zip"
  sudo apt-get install -y unzip
  unzip -o "packer_${ver}_linux_amd64.zip"
  sudo mv packer /usr/local/bin

  popd || exit 1
fi


packer build rack-box-docker.json
docker image ls
