#!/bin/sh
set -eu

if ! command -v rack > /dev/null
then
    echo "rack cli tool not found in PATH"
    exit 1
fi

rack --base-url http://10.33.49.125 model import --clear ../OwlModels/import.yaml
rack --base-url http://10.33.49.125 model import  ../../Turnstile/99-Utils/import.yaml

rack --base-url http://10.33.49.125 data import --clear ../OwlModels/DO-178C.yaml
rack --base-url http://10.33.49.125 data import ../../Turnstile/99-Utils/Model.yaml

