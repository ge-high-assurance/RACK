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
    if [ "${exe:0:2}" == "./" ] ;
    then basename "${exe}"
    else
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
    fi
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
        [ -z "$to_make" ] && continue
        add_make_step "$to_make"
    done
)
}

# Run this prior to running a specific operation to capture the
# information about the start of that operation.
#
# Arguments:
#    $1 - tool generic name
#    $* - command-line arguments
#
# shell variables referenced:
#    nonce - the nonce value for this operation
#    tool - the tool base name (this wrapper)
#    realtool - the full path of the underlying tool being invoked

rack_info_pre() {
    what=${1}
    shift 1
    echo ":- multifile build_with/5, build_from/2, build_inputs/2, build_outputs/2, build_start/2, build_finished/3, build_step/2, build_user/2, build_on/2, file_sha1/3."
    # shellcheck disable=SC2086,SC2154
    echo "build_with(${nonce@Q}, ${what@Q}," ${tool@Q}, ${realtool@Q}, [ "${*@Q}" ] ")."
    # shellcheck disable=SC2027,SC2046
    echo "build_from(${nonce@Q}, '"$(top_rel_curdir)"')."
    # shellcheck disable=SC2027,SC2046
    echo "build_on(${nonce@Q}, '"$(hostname --fqdn)"')."
    echo "build_start(${nonce@Q}, $(date +'date_time(%Y,%m,%d,%H,%M,%S,%z)'))."
    echo "build_user(${nonce@Q}, '$(whoami)')."
    # shellcheck disable=SC2162,SC2034
    IFS=' ' read sum f < <(sha1sum "$realtool")
    echo "file_sha1(${tool@Q}, ${sum@Q}, ${nonce@Q})."
}

# Run this on completion of a specific operation to capture the
# completion information of the operation for ASSIST.

rack_info_post() {
    rval=$?
    echo "build_finished(${nonce@Q}, $(date +'date_time(%Y,%m,%d,%H,%M,%S,%z)'), $rval)."
}
