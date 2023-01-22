#!/bin/sh
set -eu
cd "$(dirname "$0")"
cd ..
echo "Entering directory '$PWD'"
set -x
rsync -crv --delete --exclude="*~" www/shared/ www/scheme.org/ \
    stag-www@scheme.org:www/
