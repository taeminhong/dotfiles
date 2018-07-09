#!/usr/bin/env bash

usage() {
    echo "usage: $(basename $0) BASE DIR SUFFIX FILE" >&2
}

create_dir() {
    if [ ! -d "$1" ]; then
        mkdir -p "$1"
    fi
}

if [ $# -lt 4 ]; then
    usage
    exit 1
fi

suffix=$3
src=$4
dest=$2/$(realpath -m --relative-base="$1" "${src%$suffix}")
create_dir $(dirname "$dest")
if [ -f "$dest" ]; then
    echo >>"$dest"
fi
cat $src >>"$dest"
