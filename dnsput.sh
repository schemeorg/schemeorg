#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
curl \
    --fail \
    --silent \
    --show-error \
    --request PUT \
    --header "X-Api-Key: $GANDI" \
    --header "Content-Type: text/plain" \
    --data-binary @dns.zone \
    https://dns.api.gandi.net/api/v5/domains/scheme.org/records
