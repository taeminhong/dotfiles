#!/bin/sh

mode=
all=

usage () {
    echo "usage: git fix [--amend|--reword] [<commit>]" >&2
}

is_rebasing () {
    git status | grep -q 'rebase in progress'
}

rebase_fix () {
    hint="hint: To abort, run \"git rebase --abort && \
git reset --soft $(git rev-parse --short HEAD)\""
    old_head="$(git rev-parse --short HEAD)"
    git commit ${all:+-a} --fixup="${mode:+${mode}:}$1" &&
        git rebase -i "$1~" ||
        (echo "$hint" >&2 && false)
}

while test $# != 0
do
    case "$1" in
        --help | -h)
            usage
            exit 0
            ;;
        --amend | --reword)
            mode="${1##*-}"
            ;;
        --all | -a)
            all=1
            ;;
        -*)
            usage
            exit 1
            ;;
        *)
            break
            ;;
    esac
    shift
done

if test "$(git rev-parse ${1-HEAD})" = "$(git rev-parse HEAD)"
then
    case "$mode" in
        amend)
            git commit ${all:+-a} --amend -cHEAD
            ;;
        reword)
            git commit --amend --only
            ;;
        *)
            git commit ${all:+-a} --amend -CHEAD
            ;;
    esac
else
    if is_rebasing
    then
        echo "fetal: Cannot ${mode:-fixup} a past commit while rebasing." >&2
    else
        commit="$(git rev-parse "$1")" && rebase_fix "$commit"
    fi
fi
