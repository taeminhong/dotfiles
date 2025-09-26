die_with_status () {
    status="$1"
    shift
    printf >&2 '%s\n' "$*"
    exit "$status"
}

die () {
    die_with_status 1 "$@"
}

usage () {
    die_with_status "${1:-1}" "usage: $USAGE"
}
