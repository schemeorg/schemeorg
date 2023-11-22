#!/bin/sh
set -eu
cd "$(dirname "$0")"
cd ..
echo "Entering directory '$PWD'"
set -x
rsync -crv --delete --exclude="*~" www/shared/ www/scheme.org/ \
    scheme.org:/production/www/www/
rsync -crv --delete --exclude="*~" www/shared/ www/redirect.scheme.org/ \
    scheme.org:/production/redirect/www/
