#!/usr/bin/env bash
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu
BASEDIR=$(cd "$(dirname "$0")"; pwd)
echo "$BASEDIR"
TOPDIR="${BASEDIR}/.."
ASSISTDIR="${TOPDIR}/assist"

cd "$BASEDIR"/Turnstile-IngestionPackage/CounterApplicationImplementation
export PATH="${ASSISTDIR}/bin:${ASSISTDIR}/databin:${PATH}"
make clean
make
make test
make dist
ingest_data -r "${BASEDIR}"/Turnstile-IngestionPackage/turnstile-ingest.rack \
            -O "${TOPDIR}"/GE-Ontology \
            -O "${TOPDIR}"/RACK-Ontology \
            -o "${BASEDIR}"/Turnstile-IngestionPackage/BuildProcess/turnstile-build.owl \
            http://rack001/turnstiledata .

cd "$BASEDIR"
rack manifest build Turnstile-IngestionPackage/manifest.yaml turnstile-ingestion-package
