#!/bin/sh
set -eu

domain="$1"
case "${domain}" in
scheme.org) ;;
schemers.org) ;;
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
    --fail \
    --silent \
    --show-error \
    --request GET \
    --header "X-Api-Key: $GANDI" \
    --header "Accept: text/plain" \
    --output "dns/${domain}.zone" \
    "https://dns.api.gandi.net/api/v5/domains/${domain}/records"
