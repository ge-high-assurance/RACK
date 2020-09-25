#!/bin/bash

# Exit if anything goes wrong

set -exo pipefail
cd /tmp/files

# Execute this part of the script only if we're building a Docker image

if [ "${PACKER_BUILDER_TYPE}" == "docker" ]; then

    # Install necessary packages

    export DEBIAN_FRONTEND=noninteractive
    export DEBCONF_NONINTERACTIVE_SEEN=true

    apt-get update -y
    apt-get install -y curl default-jre gettext-base nginx-light python3 unzip

    # Install docker-systemctl-replaement

    chmod 755 systemctl3.py
    mv systemctl3.py /usr/bin
    ln -sf systemctl3.py /usr/bin/systemctl

    # Create ubuntu user

    adduser --disabled-password --gecos '' ubuntu

fi

# Unpack the Fuseski distribution

tar xfzC apache-jena-fuseki-3.16.0.tar.gz /opt
mv /opt/apache-jena-fuseki-3.16.0 /opt/fuseki

# Set up and start Fuseki system service

adduser --system --group --no-create-home --disabled-password fuseki
mkdir /etc/fuseki
chown fuseki.fuseki /etc/fuseki
cp /opt/fuseki/fuseki.service /etc/systemd/system/fuseki.service
systemctl enable fuseki
systemctl start fuseki

# Unpack the SemTK distribution

export USER=ubuntu
tar xfzC semtk-opensource-2.2.1-SNAPSHOT-bin.tar.gz /home/${USER}
source ENV_OVERRIDE
mv ENV_OVERRIDE /home/${USER}/semtk-opensource
cd /home/${USER}/semtk-opensource

# Initialize SemTK environment variables

source .env

# Wait for Fuseki

apt-get install -y netcat
function wait_for_port() {
  local count=0
  for count in {1..10}; do
    if ! nc -z localhost "${1}"; then
      echo "[${count}/10] Service ${2:-} not yet available on port ${1}, sleeping..."
      sleep 20
    else
      echo "[${count}/10] Service ${2:-} is now available on port ${1}, continuing..."
      return
    fi
  done
  echo "Service ${2:-} was not available!"
  for log in /var/log/journal/"${2:-}"*.log; do
    cat "${log}"
  done
  exit 1
}

wait_for_port "3030" "Fuseki"

# Create RACK dataset
curl -Ss -d 'dbName=RACK' -d 'dbType=tdb' 'http://localhost:3030/$/datasets'

# Install the RACK landing page and SemTK webapps

export WEBAPPS=/var/www/html
./updateWebapps.sh ${WEBAPPS}
mv /tmp/files/{documentation.html,index.html,style.css} ${WEBAPPS}

# Set up each SemTK system service, in dependency order.

function create_service() {
  pushd "${1}"
  unzip -q target/*.jar
  rm -rf pom.xml target
  envsubst <../service.unit >/etc/systemd/system/"${1}".service
  systemctl enable "${1}".service
  popd || exit 1
}

for service in *Service; do
  create_service "${service}"
done

# Change file ownerships since all SemTK code runs as non-root ${USER}

chown -R ${USER}.${USER} /home/${USER}
chown -R ${USER}.${USER} ${WEBAPPS}

# Configure the SemTK services and webapps

envsubst <configSemTK.service >/etc/systemd/system/configSemTK.service
systemctl enable configSemTK
systemctl start configSemTK

# sparqlQueryService requires this gets started first

function start_service() {
  systemctl start "${1}Service".service
  wait_for_port "${2}" "${1}"
}

start_service ontologyInfo "${PORT_ONTOLOGYINFO_SERVICE:-12057}" &

# For the others, no dependencies are yet known.
pids=""
start_service sparqlQuery "${PORT_SPARQL_QUERY_SERVICE:-12050}" &
pids="${pids} $!"
start_service sparqlGraphStatus "${PORT_SPARQLGRAPH_STATUS_SERVICE:-12051}" &
pids="${pids} $!"
start_service sparqlGraphResults "${PORT_SPARQLGRAPH_RESULTS_SERVICE:-12052}" &
pids="${pids} $!"
start_service hive "${PORT_HIVE_SERVICE:-12055}" &
pids="${pids} $!"
start_service sparqlExtDispatch "${PORT_DISPATCH_SERVICE:-12053}" &
pids="${pids} $!"
start_service nodeGroupStore "${PORT_NODEGROUPSTORE_SERVICE:-12056}" &
pids="${pids} $!"
start_service nodeGroupExecution "${PORT_NODEGROUPEXECUTION_SERVICE:-12058}" &
pids="${pids} $!"
start_service nodeGroup "${PORT_NODEGROUP_SERVICE:-12059}" &
pids="${pids} $!"
start_service utility "${PORT_UTILITY_SERVICE:-12060}" &
pids="${pids} $!"
start_service sparqlGraphIngestion "${PORT_INGESTION_SERVICE:-12091}" &
pids="${pids} $!"
start_service athena "${PORT_ATHENA_SERVICE:-12062}" &
pids="${pids} $!"
start_service edcQueryGeneration "${PORT_EDCQUERYGEN_SERVICE:-12054}" &
pids="${pids} $!"
start_service arangoDb "${PORT_ARANGODB_SERVICE:-12065}" &
pids="${pids} $!"
start_service fdcSample "${PORT_FDCSAMPLE_SERVICE:-12066}" &
pids="${pids} $!"
start_service fdcCache "${PORT_FDCCACHE_SERVICE:-12068}" &
pids="${pids} $!"

for p in ${pids}; do
  wait "${p}"
done
unset pids p

# Initialize the RACK database using the RACK CLI

pushd /tmp/files
mkdir rack
mv rack.tar.gz rack
pushd rack
tar xf rack.tar.gz
pushd RACK-Ontology/cli
apt-get update
apt-get install -y python3 python3-virtualenv git # TODO: Install outside of a virtualenv
virtualenv venv
source venv/bin/activate
pip install -r requirements.txt
python3 setup.py --quiet install

# TODO: Use setup-rack.sh
rack model import ../OwlModels/import.yaml
rack nodegroups import ../../nodegroups/ingestion
rack nodegroups import ../../nodegroups/queries
rack data import --clear ../models/TurnstileSystem/Data/import.yaml
rack data import ../OwlModels/requirements.yaml

popd || exit 1
popd || exit 1
