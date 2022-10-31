#!/usr/bin/env bash
# Copyright (c) 2022, General Electric Company and Galois, Inc.

set -eu

echo "Stopping Fuseki"
systemctl stop fuseki

RACK_DB="/etc/fuseki/databases/RACK"
RACK_FUSEKI_CONFIG="/etc/fuseki/configuration/RACK.ttl"
# Currently using TDB1, this should become tdb2.tdbstats for TDB2
TDBSTATS="/opt/jena/bin/tdbstats"
TMP_STATS="/tmp/stats.opt"
# Usually it goes into a `Data-0001` subdirectory, but it seems that with our
# RACK.ttl configuration we just don't have the extra directory.
STATS_DIR="${RACK_DB}"
STATS="${STATS_DIR}/stats.opt"

if [[ ! -x "${TDBSTATS}" ]]; then
    echo "Aborting, ${TDBSTATS} is not executable"
    exit 1
fi

echo "Running tdbstats"
mkdir -p "${STATS_DIR}"
# NOTE: Jena documentation mandates using a temporary file
"${TDBSTATS}" --desc "${RACK_FUSEKI_CONFIG}" > "${TMP_STATS}" && mv "${TMP_STATS}" "${STATS}"

echo "Now printing the contents of stats.opt:"
cat "${STATS}"

echo "Restarting Fuseki"
systemctl start fuseki