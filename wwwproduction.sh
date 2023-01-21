#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
rsync -crv --delete --exclude="*~" scheme.org/www/ scheme.org:/production/www/www/
