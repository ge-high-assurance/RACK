#!/usr/bin/env bash
# Copyright (c) 2023, General Electric Company and Galois, Inc.

set -e

FUSEKI_PID=$(systemctl show --property MainPID fuseki)
FUSEKI_PID=${FUSEKI_PID#"MainPID="}

if [ -n "${FUSEKI_PID}" ]; then
    systemctl stop fuseki
    # systemctl doesn't always seem to succeed, so be quite certain
    # until we figure out why it doesn't always succeed
    kill -9 "${FUSEKI_PID}" > /dev/null 2>&1
fi

systemctl start fuseki
