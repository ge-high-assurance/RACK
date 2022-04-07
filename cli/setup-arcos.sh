#!/bin/sh
# Copyright (c) 2020, General Electric Company and Galois, Inc.
set -eu

./ensure-cli-in-PATH.sh

# Boeing model & auto-generated nodegroups
rack model import ../Boeing-Ontology/OwlModels/import.yaml
rack nodegroups import ../nodegroups/ingestion/arcos.AH-64D

# GrammaTech model & auto-generated nodegroups
rack model import ../GrammaTech-Ontology/OwlModels/import.yaml
rack nodegroups import ../nodegroups/ingestion/arcos.acert

# LM model & auto-generated nodegroups
rack model import ../LM-Ontology/OwlModels/import.yaml
rack nodegroups import ../nodegroups/ingestion/arcos.certgate

# SRI model & auto-generated nodegroups
rack model import ../SRI-Ontology/OwlModels/import.yaml
rack nodegroups import ../nodegroups/ingestion/arcos.descert

# STR model & auto-generated nodegroups
rack model import ../STR-Ontology/OwlModels/import.yaml
rack nodegroups import ../nodegroups/ingestion/arcos.arbiter

# RTX model & auto-generated nodegroups
rack model import ../RTX-Ontology/OwlModels/import.yaml
rack nodegroups import ../nodegroups/ingestion/arcos.aace

### Applicable Standards ### copy any of these to the instance data load script and use when applicable
# rack data import --clear ../RACK-Ontology/OwlModels/ARP-4754A.yaml     # from datagraph http://rack001/arp-4754a
# rack data import --clear ../RACK-Ontology/OwlModels/DO-330.yaml        # from datagraph http://rack001/do-330
# rack data import --clear ../RACK-Ontology/OwlModels/DO-178C.yaml         # from datagraph http://rack001/do-178c

rack model import ../RACK-Ontology/OwlModels/MITRE-CWE.yaml
rack nodegroups import ../nodegroups/arcos.cwe
rack data import --clear ../RACK-Ontology/ontology/MITRE-CWE/import.yaml # datagraph http://rack001/mitre-cwe
