#!/bin/sh
# Copyright (c) 2021, General Electric Company and Galois, Inc.

set -e

RACK_DIR=$(realpath "$(dirname "$0")"/..)
DOCKER_TAG="v9.0"
MODE=copy

usage() {
        echo "Usage: setup-owl.sh [-h] [-b] [-t TAG]"
        echo "   -h    print help"
        echo "   -b    build OWL files using Web SADL"
        echo "   -t    specify rack-box tag when copying OWL files from running image"
        exit 1
}

args=`getopt hbt: $*` ; errcode=$?; set -- $args
if [ $? != 0 ]; then
        usage
fi

for i
do
        case "$i"
        in
                -b) MODE="build"; shift; break;;
                -t) DOCKER_TAG="$2"; shift; shift; break;;
                --) shift; break;;
                *) usage;;
        esac
done

case "$MODE"
in
    build)
        docker run --rm -u 0 -e RUN_AS="$(id -u) $(id -g)" -v "$RACK_DIR:/RACK" sadl/sadl-eclipse:v3.5.0-20211204 -importAll /RACK -cleanBuild
        break;;

    copy)
        CONTAINER=$(docker container ls -qf ancestor=gehighassurance/rack-box:"$DOCKER_TAG")

        if [ -z "$CONTAINER" ]; then
                echo "Unable to find docker container rack-box:$DOCKER_TAG. Specify image tag with -t flag"
                exit 1
        fi

        echo "Found container $CONTAINER; copying OwlModels"

        docker cp "$CONTAINER:home/ubuntu/RACK/RACK-Ontology/OwlModels/" "$RACK_DIR/RACK-Ontology/OwlModels/"
        docker cp "$CONTAINER:home/ubuntu/RACK/GE-Ontology/OwlModels/" "$RACK_DIR/GE-Ontology/OwlModels/"
        docker cp "$CONTAINER:home/ubuntu/RACK/GrammaTech-Ontology/OwlModels/" "$RACK_DIR/GrammaTech-Ontology/OwlModels/"
        docker cp "$CONTAINER:home/ubuntu/RACK/STR-Ontology/OwlModels/" "$RACK_DIR/STR-Ontology/OwlModels/"
        docker cp "$CONTAINER:home/ubuntu/RACK/Boeing-Ontology/OwlModels/" "$RACK_DIR/Boeing-Ontology/OwlModels/"
        docker cp "$CONTAINER:home/ubuntu/RACK/LM-Ontology/OwlModels/" "$RACK_DIR/LM-Ontology/OwlModels/"
        docker cp "$CONTAINER:home/ubuntu/RACK/SRI-Ontology/OwlModels/" "$RACK_DIR/SRI-Ontology/OwlModels/"
        docker cp "$CONTAINER:home/ubuntu/RACK/Turnstile-Example/Turnstile-IngestionPackage/CounterApplicationUnitTesting/OwlModels/" "$RACK_DIR/Turnstile-Example/Turnstile-IngestionPackage/CounterApplicationUnitTesting/OwlModels/"
        break;;
esac
