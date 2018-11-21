#!/bin/sh

usage() {
    printf "usage: %s source target\n" "$(basename $0)"
}

if [ $# -lt 2 ]; then
    usage >&2
    exit 1
fi

target_dir=$(dirname "$2")
mkdir -p "$target_dir" && cat "$1" >> "$2"
