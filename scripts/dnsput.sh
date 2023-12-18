#!/bin/sh
set -eu

domain="$1"
case "${domain}" in
scheme.org) ;;
schemers.org) ;;
schemeworkshop.org) ;;
*)
    echo "Invalid domain."
    exit 1
    ;;
esac

cd "$(dirname "$0")"
cd ..
echo "Entering directory '$PWD'"
set -x
curl \
    --silent \
    --show-error \
    --request PUT \
    --header "X-Api-Key: $GANDI" \
    --header "Content-Type: text/plain" \
    --data-binary "@dns/${domain}.zone" \
    "https://dns.api.gandi.net/api/v5/domains/${domain}/records"
