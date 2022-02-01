#!/bin/sh
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu

./ensure-cli-in-PATH.sh

# --------------------
# Similar to setup-rack.sh but without clearing all the other data-graphs.
# (This is in case data from the other graphs are still needed.)
rack data clear --data-graph http://rack001/turnstiledata

rack model import ../RACK-Ontology/OwlModels/import.yaml

rack nodegroups import ../nodegroups/ingestion/arcos.rack

rack nodegroups import ../nodegroups/queries

# --------------------
# This is where turnstile overlay and ingest nodegroups get loaded
rack model import ../GE-Ontology/OwlModels/import.yaml

rack nodegroups import ../nodegroups/ingestion/arcos.turnstile


### Applicable Standards ### copy any of these to the instance data load script and use when applicable
# rack data import --clear ../RACK-Ontology/ontology/ARP-4754A/import.yaml  # from datagraph http://rack001/arp-4754a
# rack data import --clear ../RACK-Ontology/ontology/DO-330/import.yaml     # from datagraph http://rack001/do-330
rack data import --clear ../RACK-Ontology/ontology/DO-178C/import.yaml    # from datagraph http://rack001/do-178c
# rack data import --clear ../RACK-Ontology/OwlModels/MIL-STD-881D.yaml  # from datagraph http://rack001/mil-std-881d
