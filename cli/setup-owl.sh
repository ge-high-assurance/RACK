#!/bin/sh
# Copyright (c) 2021, General Electric Company and Galois, Inc.

set -e

rack_dir=$(realpath "$(dirname "$0")"/..)

rack_image="gehighassurance/rack-box"
rack_tag="v9.0"

sadl_image="sadl/sadl-eclipse"
sadl_tag="v3.5.0-20211204"

mode="copy"

usage() {
        echo "Usage: setup-owl.sh [-h] [-b] [-n NAME] [-t TAG]"
        echo "   -h    print help"
        echo "   -b    build OWL files using sadl-eclipse"
        echo "   -n    specify rack-box docker image name"
        echo "   -t    specify rack-box tag when copying OWL files from running image"
        exit 1
}

while getopts "bn:t:" o; do
        case "$o"
        in
                b) mode="build";;
                n) rack_image="${OPTARG}";;
                t) rack_tag="${OPTARG}";;
                *) usage;;
        esac
done

case "${mode}"
in
    build)
        docker run --rm -u 0 -e RUN_AS="$(id -u) $(id -g)" -v "${rack_dir}:/RACK" ${sadl_image}:${sadl_tag} -importAll /RACK -cleanBuild
        ;;

    copy)
        container=$(docker container ls -qf "ancestor=${rack_image}:${rack_tag}")

        if [ -z "${container}" ]; then
                echo "Unable to find docker container ${rack_image}:${rack_tag}."
                echo "Note: image name and image tag can be specified with -n and -t, respectively."
                exit 1
        fi

        echo "Found container ${container}"

        echo "Copying OwlModels"
        docker cp "${container}:home/ubuntu/RACK/RACK-Ontology/OwlModels/" "${rack_dir}/RACK-Ontology/"
        docker cp "${container}:home/ubuntu/RACK/GE-Ontology/OwlModels/" "${rack_dir}/GE-Ontology/"
        docker cp "${container}:home/ubuntu/RACK/GrammaTech-Ontology/OwlModels/" "${rack_dir}/GrammaTech-Ontology/"
        docker cp "${container}:home/ubuntu/RACK/STR-Ontology/OwlModels/" "${rack_dir}/STR-Ontology/"
        docker cp "${container}:home/ubuntu/RACK/Boeing-Ontology/OwlModels/" "${rack_dir}/Boeing-Ontology/"
        docker cp "${container}:home/ubuntu/RACK/LM-Ontology/OwlModels/" "${rack_dir}/LM-Ontology/"
        docker cp "${container}:home/ubuntu/RACK/SRI-Ontology/OwlModels/" "${rack_dir}/SRI-Ontology/"
        docker cp "${container}:home/ubuntu/RACK/Turnstile-Example/Turnstile-IngestionPackage/CounterApplicationUnitTesting/OwlModels/" "${rack_dir}/Turnstile-Example/Turnstile-IngestionPackage/CounterApplicationUnitTesting/"

        echo "Copying nodegroups"
        docker cp "${container}:home/ubuntu/RACK/nodegroups/CDR/" "${rack_dir}/nodegroups/"
        docker cp "${container}:home/ubuntu/RACK/nodegroups/ingestion/" "${rack_dir}/nodegroups/"
        ;;
esac
