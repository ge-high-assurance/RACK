#!/bin/bash


/home/ubuntu/RACK/Doc-Processor/setup-rack.sh
BASEDIR=$(dirname "$0")
echo "$BASEDIR"

cd $BASEDIR
python3 ./DocProcessor.py
