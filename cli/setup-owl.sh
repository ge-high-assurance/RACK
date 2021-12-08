#!/bin/sh
# Copyright (c) 2021, General Electric Company and Galois, Inc.

set -e

RACK_DIR=$(realpath "$(dirname "$0")"/..)
DOCKER_TAG="v9.0"
MODE=copy

usage() {
        echo "Usage: setup-owl.sh [-h] [-b] [-t TAG]"
        echo "   -h    print help"
        echo "   -b    build OWL files using sadl-eclipse"
        echo "   -t    specify rack-box tag when copying OWL files from running image"
        exit 1
}

while getopts "bt:" o; do
        case "$o"
        in
                b) MODE="build"; break;;
                t) DOCKER_TAG="$OPTARG"; break;;
                *) usage;;
        esac
done

case "$MODE"
in
    build)
        docker run --rm -u 0 -e RUN_AS="$(id -u) $(id -g)" -v "$RACK_DIR:/RACK" sadl/sadl-eclipse:v3.5.0-20211204 -importAll /RACK -cleanBuild
        ;;

    copy)
        CONTAINER=$(docker container ls -qf ancestor=gehighassurance/rack-box:"$DOCKER_TAG")

        if [ -z "$CONTAINER" ]; then
                echo "Unable to find docker container rack-box:$DOCKER_TAG. Specify image tag with -t flag"
                exit 1
        fi

        echo "Found container $CONTAINER"

        echo "Copying OwlModels"
        docker cp "$CONTAINER:home/ubuntu/RACK/RACK-Ontology/OwlModels/" "$RACK_DIR/RACK-Ontology/"
        docker cp "$CONTAINER:home/ubuntu/RACK/GE-Ontology/OwlModels/" "$RACK_DIR/GE-Ontology/"
        docker cp "$CONTAINER:home/ubuntu/RACK/GrammaTech-Ontology/OwlModels/" "$RACK_DIR/GrammaTech-Ontology/"
        docker cp "$CONTAINER:home/ubuntu/RACK/STR-Ontology/OwlModels/" "$RACK_DIR/STR-Ontology/"
        docker cp "$CONTAINER:home/ubuntu/RACK/Boeing-Ontology/OwlModels/" "$RACK_DIR/Boeing-Ontology/"
        docker cp "$CONTAINER:home/ubuntu/RACK/LM-Ontology/OwlModels/" "$RACK_DIR/LM-Ontology/"
        docker cp "$CONTAINER:home/ubuntu/RACK/SRI-Ontology/OwlModels/" "$RACK_DIR/SRI-Ontology/"
        docker cp "$CONTAINER:home/ubuntu/RACK/Turnstile-Example/Turnstile-IngestionPackage/CounterApplicationUnitTesting/OwlModels/" "$RACK_DIR/Turnstile-Example/Turnstile-IngestionPackage/CounterApplicationUnitTesting/"

        echo "Copying nodegroups"
        docker cp "$CONTAINER:home/ubuntu/RACK/nodegroups/CDR/" "$RACK_DIR/nodegroups/"
        docker cp "$CONTAINER:home/ubuntu/RACK/nodegroups/ingestion/" "$RACK_DIR/nodegroups/"
        ;;
esac
