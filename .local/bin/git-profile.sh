#!/bin/sh

USAGE='git profile [-h|--help] [-l|--list] [--unset] [PROFILE]'

. "$(dirname $0)/sh-setup.sh"

set -e

list_profiles () {
    current=
    email="$(git config user.email)"
    case "$email" in
        "taeminhong@outlook.com")
            current=taemin
            ;;
        "kyle.hong@z-emotion.com")
            current=kyle
            ;;
    esac

    all='  taemin
  kyle'

    if test -z "$current"
    then
        echo "* (Unknown: $email)"
    fi
    echo '  taemin
  kyle' | sed 's/^  \('"$current"'\)$/* \1/'
}

# Check if we are in a repository
git rev-parse --git-dir >/dev/null

while test $# -ne 0 && test -z "$profile"
do
    case "$1" in
        -h | --help)
            usage 0
            ;;
        -l | --list)
            list_profiles
            exit 0
            ;;
        --unset)
            git config --local --unset user.name
            git config --local --unset user.email
            exit 0
            ;;
        *)
            profile="$1"
            ;;
    esac
    shift
done

if test -z "$profile"
then
    list_profiles
    exit 0
fi

case "$profile" in
    taemin | taeminhong | taemin.hong)
        git config --local user.name "Taemin Hong"
        git config --local user.email "taeminhong@outlook.com"
        ;;
    kyle | kylehong | kyle.hong)
        git config --local user.name "Kyle Hong"
        git config --local user.email "kyle.hong@z-emotion.com"
        ;;
    *)
        die "Unknown profile: $profile"
        ;;
esac
