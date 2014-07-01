#!/bin/sh -efu

REPO=git://git.altlinux.org/people/manowar/public/etcgit-config-repo.git
DIR="defaults.git"

git clone --bare "$REPO" "$DIR"
tar cf "${DIR%/}.tar" "$DIR"
rm -rf "$DIR"
