#!/bin/bash

# Exit if anything goes wrong

set -eo pipefail
export USER=${1:-ubuntu}
cd /tmp/files

# Install necessary packages non-interactively

export DEBIAN_FRONTEND=noninteractive
export DEBCONF_NONINTERACTIVE_SEEN=true
apt-get update -yqq
apt-get install -yqq ca-certificates software-properties-common
cp GE_External_Root_CA_2_1.crt /usr/local/share/ca-certificates
update-ca-certificates
add-apt-repository -yu ppa:swi-prolog/stable
apt-get update -yqq

# If you change packages here, change them in rack-box/http/user-data too

apt-get install -yqq \
        curl \
        default-jre \
        gettext-base \
        nano \
        nginx-light \
        python3 \
        python3-pip \
        strace \
        sudo \
        swi-prolog \
        unzip \
        vim

# Ensure we can log into the vm like we used to

if getent passwd vagrant >/dev/null && [ -f "/etc/ssh/sshd_config" ]; then
    sed -i -e "s/.*PasswordAuthentication.*/PasswordAuthentication yes/g" /etc/ssh/sshd_config
    echo "ubuntu:ubuntu" | chpasswd
fi

# Execute this part of the script only if we're building a Docker image

if [ "${PACKER_BUILDER_TYPE}" == "docker" ]; then

    # Install docker-systemctl-replaement

    chmod 755 systemctl3.py
    mv systemctl3.py /usr/bin
    ln -sf systemctl3.py /usr/bin/systemctl

    # Create user

    adduser --disabled-password --gecos '' "${USER}"

fi

# Unpack Fuseki, RACK, and SemTK distributions

mkdir -p "/home/${USER}"
tar xfzC fuseki.tar.gz /opt
rm fuseki.tar.gz
mv /opt/apache-jena-fuseki-* /opt/fuseki
tar xfzC jena.tar.gz /opt
rm jena.tar.gz
mv /opt/apache-jena-* /opt/jena
tar xfzC rack.tar.gz "/home/${USER}"
rm rack.tar.gz
tar xfzC rack-assist.tar.gz "/home/${USER}"
rm rack-assist.tar.gz
tar xfzC rack-cli.tar.gz "/home/${USER}"
rm rack-cli.tar.gz
tar xfzC rack-ui.tar.gz "/home/${USER}"
rm rack-ui.tar.gz
tar xfzC semtk.tar.gz "/home/${USER}"
rm semtk.tar.gz
mv ENV_OVERRIDE "/home/${USER}/semtk-opensource"

# Set up and start Fuseki system service

adduser --system --group --no-create-home --disabled-password fuseki
mkdir /etc/fuseki
chown fuseki.fuseki /etc/fuseki
cp /opt/fuseki/fuseki.service /etc/systemd/system/fuseki.service
systemctl enable fuseki
systemctl start fuseki

# Set up and start RACK UI service

cd /home/"${USER}"/RACK/rack-ui
python3 -m pip install -r ./requirements.txt
adduser --system --group --no-create-home --disabled-password rackui
usermod -aG sudo rackui
echo "rackui ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/rackui
mkdir /etc/rackui
chown rackui.rackui /etc/rackui
envsubst < rackui.service > /etc/systemd/system/rackui.service
systemctl enable rackui

# Initialize SemTK environment variables

cd "/home/${USER}/semtk-opensource"
# This lets rackui see and run scripts that live in /home/ubuntu/RACK/cli
chmod 755 "/home/${USER}"
chmod 755 ./*.sh
export SERVER_ADDRESS=localhost
export SERVICE_HOST=localhost
# shellcheck disable=SC1091
source .env

# Set up each SemTK system service

for service in ${ENABLED_SERVICES}; do
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
mkdir -p "${WEBAPPS}"
./updateWebapps.sh "${WEBAPPS}"
mv /tmp/files/{documentation.html,index.html,style.css} "${WEBAPPS}"

# Change file ownerships since all SemTK code runs as non-root ${USER}

chown -R "${USER}.${USER}" "/home/${USER}"
chown -R "${USER}.${USER}" "${WEBAPPS}"

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

for service in ${ENABLED_SERVICES}; do
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

# Setup the RACK dataset using the RACK CLI.  Make cli scripts executable by RACK UI.

cd "/home/${USER}/RACK/cli/"
chmod 755 ./*.sh
python3 -m pip install ./wheels/*.whl
./setup-rack.sh
