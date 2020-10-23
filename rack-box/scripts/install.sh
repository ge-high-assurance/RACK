#!/bin/bash

# Exit if anything goes wrong

set -eo pipefail
export USER=ubuntu
cd /tmp/files

# Execute this part of the script only if we're building a Docker image

if [ "${PACKER_BUILDER_TYPE}" == "docker" ]; then

    # Install necessary packages

    export DEBIAN_FRONTEND=noninteractive
    export DEBCONF_NONINTERACTIVE_SEEN=true

    apt-get update -yqq
    apt-get install -yqq curl default-jre gettext-base nano nginx-light python3 software-properties-common unzip

    # Install docker-systemctl-replaement

    chmod 755 systemctl3.py
    mv systemctl3.py /usr/bin
    ln -sf systemctl3.py /usr/bin/systemctl

    # Create user

    adduser --disabled-password --gecos '' $USER

fi

# Unpack Fuseki, RACK, and SemTK distributions

tar xfzC fuseki.tar.gz /opt
rm fuseki.tar.gz
mv /opt/apache-jena-fuseki-3.16.0 /opt/fuseki
tar xfzC rack.tar.gz /home/${USER}
rm rack.tar.gz
tar xfzC semtk.tar.gz /home/${USER}
rm semtk.tar.gz
mv ENV_OVERRIDE /home/${USER}/semtk-opensource

# Set up and start Fuseki system service

adduser --system --group --no-create-home --disabled-password fuseki
mkdir /etc/fuseki
chown fuseki.fuseki /etc/fuseki
cp /opt/fuseki/fuseki.service /etc/systemd/system/fuseki.service
systemctl enable fuseki
systemctl start fuseki

# Initialize SemTK environment variables

cd /home/${USER}/semtk-opensource
chmod 755 ./*.sh
export SERVER_ADDRESS=localhost
export SERVICE_HOST=localhost
# shellcheck disable=SC1091
source .env

# Set up each SemTK system service

for service in *Service; do
  (
    cd "${service}"
    unzip -q target/*.jar
    rm -rf pom.xml target
    envsubst <../service.unit >/etc/systemd/system/"${service}".service
    systemctl enable "${service}"
  )
done

# Install the RACK landing page and SemTK webapps

export WEBAPPS=/var/www/html
./updateWebapps.sh ${WEBAPPS}
mv /tmp/files/{documentation.html,index.html,style.css} ${WEBAPPS}

# Change file ownerships since all SemTK code runs as non-root ${USER}

chown -R ${USER}.${USER} /home/${USER}
chown -R ${USER}.${USER} ${WEBAPPS}

# Wait for Fuseki to become ready

echo "Waiting for Fuseki at http://localhost:3030..."
MAX_SECS=400
while ! curl http://localhost:3030/$/ping &>/dev/null; do
    if [[ $SECONDS -gt $MAX_SECS ]]; then
        echo "Error: Took longer than $MAX_SECS seconds to start Fuseki"
        exit 1
    fi
    sleep 10
done

# Create the RACK dataset

curl -Ss -d 'dbName=RACK' -d 'dbType=tdb' 'http://localhost:3030/$/datasets'

# Configure the SemTK services and webapps

envsubst <configSemTK.service >/etc/systemd/system/configSemTK.service
systemctl enable configSemTK
systemctl start configSemTK

# sparqlQueryService requires that ontologyInfoService be started first

systemctl start ontologyInfoService

# For the rest of the services, no start order dependencies are known

for service in *Service; do
  [ "${service}" != ontologyInfoService ] && systemctl start "${service}"
done

# Wait for the nodeGroupService to become ready

echo "Waiting for nodeGroupService at http://localhost:12059..."
MAX_SECS=600
while ! curl -X POST http://localhost:12059/serviceInfo/ping 2>/dev/null | grep -q yes; do
    if [[ $SECONDS -gt $MAX_SECS ]]; then
        echo "Error: Took longer than $MAX_SECS seconds to start nodeGroupService"
        exit 1
    fi
    sleep 10
done

# Setup the RACK dataset using the RACK CLI

cd /home/${USER}/RACK/RACK-Ontology/cli
# shellcheck disable=SC1091
source venv/bin/activate
./setup-rack.sh
