#!/usr/bin/env bash

default_version=v11

if [ "$1" == "--help" -o "$1" == "-?" -o "$1" == "help" ] ; then
    echo "This tool can be used to (re-)start a RACK docker image (using"
    echo "either the docker or podman commands, depending on which is"
    echo "installed).  The optional command-line argument specifies which"
    echo "version of the RACK image to start; the default is ${default_version}"
    exit 0
fi

set -e -o pipefail

image="gehighassurance/rack-box:${1:-$default_version}"

cmd_exists () { type -p $1 > /dev/null 2>&1; }
# shellcheck disable=SC1075
if cmd_exists podman
then cmd=podman
else if cmd_exists docker
     then cmd=docker
     else >&2 echo 'Cannot find docker or podman installation!'; exit 1;
     fi
fi

name=$(echo ${image} | cut -d/ -f2 | sed -e so:o-o)

containerName () {  # $1 is image name
    grep $1 <(${cmd} container ls $2 --format '{{ .Image}}~{{ .Names}}') | cut -d~ -f2
}

mapfile -t existing < <(containerName ${image} -a)

if containerName ${image} >/dev/null
then echo RACK docker image already running

elif [ 0 == ${#existing[*]} ]
then ${cmd} run --detach --name ${name} \
            -p 3030:3030 \
            -p 8050:8050 \
            -p 8080:80 \
            -p 12050-12091:12050-12091 \
            ${image}
     echo Started RACK container image: ${image}
     echo Stop container by typing:
     echo "    $ ${cmd} container stop $(containerName ${image})"

elif [ 1 == ${#existing[*]} ]
then echo Restarting stopped RACK container
     echo Note: To discard and start a fresh container, type:
     echo "   $ ${cmd} container stop ${existing}"
     echo "   $ ${cmd} container rm ${existing}"
     ${cmd} start ${existing}
else echo Multiple stopped RACK containers exist: ${existing[*]}
     echo Not sure which one to restart.  Remove extras, or manually start one via:
     echo "   $ ${cmd} container start NAME"
     false  # this is considered an error result
fi
