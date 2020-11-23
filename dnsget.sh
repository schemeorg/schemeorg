#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
curl \
    --fail \
    --silent \
    --show-error \
    --request GET \
    --header "X-Api-Key: $GANDI" \
    --header "Accept: text/plain" \
    --output dns.zone \
    https://dns.api.gandi.net/api/v5/domains/scheme.org/records
