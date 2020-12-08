#!/bin/bash

BASEDIR=$(dirname "$0")
echo "$BASEDIR"
$BASEDIR/setup-rack.sh


cd $BASEDIR
python3 $BASEDIR/DocProcessor.py
