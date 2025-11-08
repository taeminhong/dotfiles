#!/bin/sh

USAGE="git kickoff [--force] [<project>]"

force=

die () {
    echo "$@" >&2
    exit 1
}

usage () {
    echo "$USAGE"
    exit "${1:-1}"
}

is_work_tree () {
    git -C "$1" rev-parse --is-inside-work-tree >/dev/null 2>&1
}

no_commits () {
    ! git -C "$1" log -1 >/dev/null 2>&1
}

kickoff () {
    mkdir -p "$1" >/dev/null 2>&1 || die "$1 is not a directory"

    if test -n "$force" || ! is_work_tree "$1"
    then
        git -C "$1" init
        if no_commits "$1"
        then
           git -C "$1" commit --allow-empty -m "Initial empty commit"
        fi
    elif test -z "$force"
    then
        die "'$1' is already a Git repository. You can use --force option to reinitialize it."
    fi
}

while test $# -gt 0
do
    case "$1" in
        -h | --help)
            usage 0
            ;;
        -f | --force)
            force="$1"
            ;;
        --)
            shift
            break
            ;;
        -*)
            usage
            ;;
        *)
            break
            ;;
    esac
    shift
done

if test $# -eq 0
then
    kickoff .
else
    for proj in "$@"
    do
        kickoff "$proj"
    done
fi
