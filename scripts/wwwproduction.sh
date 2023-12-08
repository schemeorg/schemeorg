#!/bin/sh
set -eu
cd "$(dirname "$0")"
cd ..
echo "Entering directory '$PWD'"
set -x
rsync -crv --delete --exclude="*~" www/shared/ www/scheme.org/ \
    scheme.org:/var/www/www.scheme.org/
rsync -crv --delete --exclude="*~" www/shared/ www/redirect.scheme.org/ \
    scheme.org:/var/www/redirect.scheme.org/
