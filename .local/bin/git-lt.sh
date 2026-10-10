#!/bin/sh

# Show a graph from latest annotated tag

commit="${1:-HEAD}"
git log --graph --oneline --abbrev-commit $(git latest-tag "$commit"~).."$commit"
