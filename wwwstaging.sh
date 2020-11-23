#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
rsync -vax --delete www/ stag-www@scheme.org:www/
