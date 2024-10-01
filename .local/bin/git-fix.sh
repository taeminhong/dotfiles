#!/bin/sh

mode=
all=
allow_empty=
commit="$(git rev-parse HEAD)" || exit $?

usage () {
    echo "usage: git fix [--amend|--reword] [<commit>]" >&2
}

is_rebasing () {
    git status | grep -q 'rebase in progress'
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
            all="-a"
            ;;
        --allow-empty)
            allow_empty="--allow-empty"
            ;;
        -*)
            usage
            exit 1
            ;;
        *)
            commit="$(git rev-parse "$1")" || exit $?
            ;;
    esac
    shift
done

if test "$commit" = "$(git rev-parse HEAD)"
then
    case "$mode" in
        amend)
            git commit $all $allow_empty --amend -cHEAD
            ;;
        reword)
            git commit --allow-empty --amend --only
            ;;
        *)
            git commit $all $allow_empty --amend -CHEAD
            ;;
    esac
else
    if is_rebasing
    then
        echo "fetal: Cannot ${mode:-fixup} a past commit while rebasing." >&2
    else
        old_head="$(git rev-parse --short HEAD)"
        git commit $all --fixup="${mode:+${mode}:}$commit" || exit $?
        if ! git rebase -i "$commit~"
        then
            status=$?
            if is_rebasing
            then
                echo "hint: To abort, run \"git rebase --abort && git reset --soft $old_head\"" >&2
            else
                git reset --soft "$old_head" >/dev/null
            fi
            exit "$status"
        fi
    fi
fi
