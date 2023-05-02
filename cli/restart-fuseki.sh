#!/usr/bin/env bash
# Copyright (c) 2023, General Electric Company and Galois, Inc.

set -e

if systemctl is-active --quiet fuseki; then

    echo "Stopping Fuseki"

    # remember the PID in case stopping fuseki doesn't stop it    
    FUSEKI_PID=$(systemctl show --property MainPID fuseki)
    FUSEKI_PID=${FUSEKI_PID#"MainPID="}

    systemctl stop fuseki

    # systemctl stop doesn't always work, so kill -9 for now to be sure
    if [ -n "${FUSEKI_PID}" ]; then
        kill -9 "${FUSEKI_PID}" &> /dev/null || true
    fi
fi

systemctl start fuseki
