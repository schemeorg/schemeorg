#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
rsync -vax --delete www/ --exclude="*~" stag-www@scheme.org:www/
