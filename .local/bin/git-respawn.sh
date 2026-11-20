#!/bin/sh

die () {
    test $# -gt 0 && echo "$@" >&2
    exit 1
}

is_merge () {
    git rev-parse "$1^2" >/dev/null 2>&1
}

has () {
    git show-ref -q --verify "refs/heads/$1"
}

is_ancestor () {
    git merge-base --is-ancestor "$1" "$2"
}

usage () {
    echo "git respawn [-h|--help] [-n|--dry-run] [-q|--quiet] [commit]"
}

while [ $# -ne 0 ]
do
    case "$1" in
        -h|--help)
            usage
            exit 0
            ;;
        -q|--quiet)
            quiet=1
            ;;
        -n|--dry-run)
            dry_run="$1"
            ;;
        --)
            shift
            break
            ;;
        -*)
            usage >&2
            exit 1
            ;;
        *)
            break
            ;;
    esac
    shift
done

commit="${1:-HEAD}"
is_merge "$commit" || die "$commit is not a merge commit"
branch=$(git show -s --format=%s "$commit" | sed -n "s/^Merge branch '\([^']*\)'.*/\1/p")
test -n "$branch" || die "Can't get branch name"
target=$(git rev-parse "$commit^2")
! has "$branch" || is_ancestor "refs/heads/$branch" "$target" || die "$branch already exists and can't fast-forward"
short_target=$(git rev-parse --short "$target")

if test -z "$dry_run"
then
    git branch -f "$branch" "$target" || die
    if test -z "$quiet"
    then
        echo "Respawn branch '$branch' to $short_target"
    fi
else
    echo "Would respawn branch '$branch' to $short_target"
fi
