#!/bin/sh
# Copyright (c) 2021, General Electric Company and Galois, Inc.
set -eu

./ensure-cli-in-PATH.sh

rack model clear

rack data clear                               \
    --data-graph http://rack001/data          \
    --data-graph http://rack001/arp-4754a     \
    --data-graph http://rack001/do-330        \
    --data-graph http://rack001/do-178c       \
    --data-graph http://rack001/mil-std-881d

rack nodegroups delete-all --yes
