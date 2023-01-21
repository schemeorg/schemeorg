#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
rsync -crv --delete scheme.org/www/ --exclude="*~" stag-www@scheme.org:www/
