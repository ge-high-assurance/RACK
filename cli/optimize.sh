#!/usr/bin/env bash
# Copyright (c) 2022, General Electric Company and Galois, Inc.

set -eu

echo "Stopping Fuseki"
FUSEKI_PID=$(systemctl show --property MainPID fuseki)
FUSEKI_PID=${FUSEKI_PID#"MainPID="}
if [ -n "${FUSEKI_PID}" ]; then
    systemctl stop fuseki
fi
# systemctl stop doesn't always work, so kill -9 for now to be sure
FUSEKI_PID=$(systemctl show --property MainPID fuseki)
FUSEKI_PID=${FUSEKI_PID#"MainPID="}
if [ -n "${FUSEKI_PID}" ]; then
    kill -9 "${FUSEKI_PID}" > /dev/null 2>&1
fi

RACK_DB="/etc/fuseki/databases/RACK"
RACK_FUSEKI_CONFIG="/etc/fuseki/configuration/RACK.ttl"
# Currently using TDB2, revert to tdbstats for TDB1
TDBSTATS="/opt/jena/bin/tdb2.tdbstats"
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
chown fuseki "${STATS}"
systemctl start fuseki
