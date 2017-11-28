#!/usr/bin/env bash

pushd $(dirname ${BASH_SOURCE})
patch -u ~/.fzf/shell/key-bindings.bash  <./key-binding.patch
popd
