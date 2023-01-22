#!/bin/sh
set -eu
cd "$(dirname "$0")"
cd ..
exec csi -R r7rs -script lib/www.scm
