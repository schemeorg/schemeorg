#!/bin/sh
set -eu

export DOMAIN=$1

case "${DOMAIN}" in
  scheme.org) ;;
  schemers.org) ;;
  *)
    echo "Invalid domain."
    exit 1
esac
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
    --output dns.scheme.org.zone \
    https://dns.api.gandi.net/api/v5/domains/scheme.org/records
