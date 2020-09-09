find_in_path_remainder() { # $1 = name of executable
    find_in_path_excluding "$1" $(dirname $0) $(dirname $0)/
}

find_in_path_excluding() { # $1 = name of executable, $2.. = exclusion paths
    exe=$1; shift 1
    (PATH=$(echo $(IFS=:
                   for P in $PATH; do
                       for excl; do
                           if [[ "$excl" == "$P" ]] ; then
                               continue 2
                           fi
                       done
                       echo $P
                   done
                ) | tr ' ' :) type -p $exe)
}

top_rel_curdir() {
    tdir=${TOPDIR:-$(pwd)}
    here=$(pwd | sed -e "s,^${tdir}/\?\(.*\),\1,")
    if [ -z $here ] ; then echo . ; else echo $here; fi
}


add_make_step() {
    export IFS="@"
    set ${1}
    make_rackfile=$1
    make_nonce=$2
    echo "build_step(${make_nonce@Q}, ${nonce@Q})." >> ${make_rackfile}
}

update_make_steps() {
(
    export IFS=":"
    for to_make in ${MAKE_DATABIN:-""} ; do
        add_make_step $to_make
    done
)
}
