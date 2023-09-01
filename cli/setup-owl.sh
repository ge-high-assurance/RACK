#!/bin/sh
# Copyright (c) 2021, General Electric Company and Galois, Inc.

set -e

rack_dir=$(realpath "$(dirname "$0")"/..)

DOCKER="${DOCKER:-docker}"

rack_image="gehighassurance/rack-box"
rack_tag="v13.0"

sadl_image="sadl/sadl-eclipse"
sadl_tag="v3.5.0-20211204"

mode="docker"

usage() {
        echo "Usage: setup-owl.sh [-h] [-b] [-n NAME] [-t TAG] [-i IPADDRESS]"
        echo "   -h    print help"
        echo "   -b    build OWL files using sadl-eclipse"
        echo "   -n    specify rack-box docker image name"
        echo "   -t    specify rack-box tag when copying OWL files from running image"
        echo "   -i    get OWL files from VirtualBox VM by IP address"
        exit 1
}

while getopts "bi:n:t:" o; do
        case "$o"
        in
                b) mode="build";;
                n) rack_image="${OPTARG}";;
                t) rack_tag="${OPTARG}";;
                i) virtualbox_ip="${OPTARG}"; mode="virtualbox";;
                *) usage;;
        esac
done

case "${mode}"
in
    build)
        echo "[setup-owl] Building OWL files using docker"

        $DOCKER run --rm -u 0 -e RUN_AS="$(id -u) $(id -g)" -v "${rack_dir}:/RACK" ${sadl_image}:${sadl_tag} -importAll /RACK -cleanBuild
        ;;

    virtualbox)
        echo "[setup-owl] Copying CDR and OWL files from VirtualBox"

        if ! ssh -i rack_ssh_key -oBatchMode=true "ubuntu@${virtualbox_ip}" true
        then
                echo "[setup-owl] Unable to authenticate to VM, attempting to install key"

                if [ ! -e rack_ssh_key ]
                then
                        echo "[setup-owl] Generating a fresh SSH keypair"
                        ssh-keygen -q -N "" -f rack_ssh_key
                fi

                echo "[setup-owl] Please confirm the SSH host key if prompted."
                echo "[setup-owl] Please enter the default password if prompted: $(tput bold)ubuntu$(tput sgr0)"

                # We're using this awkward command instead of ssh-copy-id because some old system don't have ssh-copy-id
                ssh "ubuntu@${virtualbox_ip}" "mkdir -p ~/.ssh/; cat >> ~/.ssh/authorized_keys" < rack_ssh_key.pub
        fi

        echo "[setup-owl] Copying OwlModels"
        scp -q -i rack_ssh_key -r "ubuntu@${virtualbox_ip}:RACK/RACK-Ontology/OwlModels" "${rack_dir}/RACK-Ontology/"
        scp -q -i rack_ssh_key -r "ubuntu@${virtualbox_ip}:RACK/overlays/GE-Ontology/OwlModels" "${rack_dir}/overlays/GE-Ontology/"
        scp -q -i rack_ssh_key -r "ubuntu@${virtualbox_ip}:RACK/overlays/GrammaTech-Ontology/OwlModels" "${rack_dir}/overlays/GrammaTech-Ontology/"
        scp -q -i rack_ssh_key -r "ubuntu@${virtualbox_ip}:RACK/overlays/STR-Ontology/OwlModels" "${rack_dir}/overlays/STR-Ontology/"
        scp -q -i rack_ssh_key -r "ubuntu@${virtualbox_ip}:RACK/overlays/Boeing-Ontology/OwlModels" "${rack_dir}/overlays/Boeing-Ontology/"
        scp -q -i rack_ssh_key -r "ubuntu@${virtualbox_ip}:RACK/overlays/LM-Ontology/OwlModels" "${rack_dir}/overlays/LM-Ontology/"
        scp -q -i rack_ssh_key -r "ubuntu@${virtualbox_ip}:RACK/overlays/SRI-Ontology/OwlModels" "${rack_dir}/overlays/SRI-Ontology/"
        scp -q -i rack_ssh_key -r "ubuntu@${virtualbox_ip}:RACK/overlays/RTX-Ontology/OwlModels" "${rack_dir}/overlays/RTX-Ontology/"
        scp -q -i rack_ssh_key -r "ubuntu@${virtualbox_ip}:RACK/Turnstile-Example/Turnstile-IngestionPackage/CounterApplicationUnitTesting/OwlModels" "${rack_dir}/Turnstile-Example/Turnstile-IngestionPackage/CounterApplicationUnitTesting/"
        scp -q -i rack_ssh_key -r "ubuntu@${virtualbox_ip}:RACK/sadl-examples/OwlModels" "${rack_dir}/sadl-examples/"

        echo "[setup-owl] Copying nodegroups"
        scp -q -i rack_ssh_key -r "ubuntu@${virtualbox_ip}:RACK/nodegroups/CDR" "${rack_dir}/nodegroups/"
        scp -q -i rack_ssh_key -r "ubuntu@${virtualbox_ip}:RACK/nodegroups/ingestion" "${rack_dir}/nodegroups/"

        ;;

    docker)
        echo "[setup-owl] Copying CDR and OWL files from Docker"

        container=$($DOCKER container ls -aqf "ancestor=${rack_image}:${rack_tag}")

        if [ -z "${container}" ]; then
                echo "Unable to find docker container ${rack_image}:${rack_tag}."
                echo "Note: image name and image tag can be specified with -n and -t, respectively."
                exit 1
        fi

        echo "[setup-owl] Found container ${container}"

        echo "[setup-owl] Copying OwlModels"
        $DOCKER cp "${container}:/home/ubuntu/RACK/RACK-Ontology/OwlModels/" "${rack_dir}/RACK-Ontology/"
        $DOCKER cp "${container}:/home/ubuntu/RACK/overlays/GE-Ontology/OwlModels/" "${rack_dir}/overlays/GE-Ontology/"
        $DOCKER cp "${container}:/home/ubuntu/RACK/overlays/GrammaTech-Ontology/OwlModels/" "${rack_dir}/overlays/GrammaTech-Ontology/"
        $DOCKER cp "${container}:/home/ubuntu/RACK/overlays/STR-Ontology/OwlModels/" "${rack_dir}/overlays/STR-Ontology/"
        $DOCKER cp "${container}:/home/ubuntu/RACK/overlays/Boeing-Ontology/OwlModels/" "${rack_dir}/overlays/Boeing-Ontology/"
        $DOCKER cp "${container}:/home/ubuntu/RACK/overlays/LM-Ontology/OwlModels/" "${rack_dir}/overlays/LM-Ontology/"
        $DOCKER cp "${container}:/home/ubuntu/RACK/overlays/SRI-Ontology/OwlModels/" "${rack_dir}/overlays/SRI-Ontology/"
        $DOCKER cp "${container}:/home/ubuntu/RACK/overlays/RTX-Ontology/OwlModels/" "${rack_dir}/overlays/RTX-Ontology/"
        $DOCKER cp "${container}:/home/ubuntu/RACK/Turnstile-Example/Turnstile-IngestionPackage/CounterApplicationUnitTesting/OwlModels/" "${rack_dir}/Turnstile-Example/Turnstile-IngestionPackage/CounterApplicationUnitTesting/"
        $DOCKER cp "${container}:/home/ubuntu/RACK/sadl-examples/OwlModels/" "${rack_dir}/sadl-examples/"

        echo "[setup-owl] Copying nodegroups"
        $DOCKER cp "${container}:/home/ubuntu/RACK/nodegroups/CDR/" "${rack_dir}/nodegroups/"
        $DOCKER cp "${container}:/home/ubuntu/RACK/nodegroups/ingestion/" "${rack_dir}/nodegroups/"
        ;;
esac
