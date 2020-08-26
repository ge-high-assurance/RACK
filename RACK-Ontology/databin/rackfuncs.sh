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
