#!/usr/bin/env head
# This file provides support scripts for the RACK assist databin
# functionality and is not meant to be run directly.
#
# Copyright (c) 2020, General Electric Company and Galois, Inc.

find_in_path_remainder() { # $1 = name of executable
    # shellcheck disable=SC2046
    find_in_path_excluding "$1" $(dirname "$0") $(dirname "$0")/
}

find_in_path_excluding() { # $1 = name of executable, $2.. = exclusion paths
    exe="$1"; shift 1
    # shellcheck disable=SC2046
    (PATH=$(echo $(IFS=:
                   for P in $PATH; do
                       for excl; do
                           if [[ "$excl" == "$P" ]] ; then
                               continue 2
                           fi
                       done
                       # shellcheck disable=SC2086
                       echo $P
                   done
                ) | tr ' ' :) type -p "$exe")
}

top_rel_curdir() {
    tdir=${TOPDIR:-$(pwd)}
    here=$(pwd | sed -e "s,^${tdir}/\?\(.*\),\1,")
    # shellcheck disable=SC2086
    echo ${here:-.}
}


add_make_step() {
    export IFS="@"
    # shellcheck disable=SC2086
    set ${1}
    make_rackfile="$1"
    make_nonce="$2"
    # shellcheck disable=SC2154
    echo "build_step(${make_nonce@Q}, ${nonce@Q})." >> "${make_rackfile}"
}

update_make_steps() {  # assumes nonce is set
(
    export IFS=":"
    for to_make in ${MAKE_DATABIN:-""} ; do
        add_make_step "$to_make"
    done
)
}
