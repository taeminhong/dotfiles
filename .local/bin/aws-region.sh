#!/bin/sh

usage="usage: aws region
   or: aws region PATTERN
   or: aws region [-h|--help]
   or: aws region [-a|--all]"

regions='N. Virginia     us-east-1
Ohio            us-east-2
N. California   us-west-1
Oregon          us-west-2
Osaka           ap-northeast-3
Seoul           ap-northeast-2
Singapore       ap-southeast-1
Sydney          ap-southeast-2
Tokyo           ap-northeast-1
Central         ca-central-1
Frankfurt       eu-central-1
Ireland         eu-west-1
London          eu-west-2
Paris           eu-west-3
Stockholm       eu-north-1
Sao Paulo       sa-east-1'

all=

die () {
    test -n "$1" && echo "$1" >&2
    exit 1
}

# Parse options
while test $# -gt 0
do
    case "$1" in
        -h | --help)
            echo "$usage"
            exit 0
            ;;
        -a | --all)
            all="$1"
            ;;
        --)
            shift
            break
            ;;
        -*)
            die "$usage"
            ;;
        *)
            break
            ;;
    esac
    shift
done

if test -n "$all"
then
    echo "$regions"
elif test -n "$1"
then
    echo "$regions" | grep -iE "$1" || die "No match found: $1"
else
    aws configure get region
fi
