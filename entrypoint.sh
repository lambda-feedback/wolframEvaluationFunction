#!/bin/bash

# Exit immediately if a command exits with a non-zero status.
# set -e

# Prevent errors in a pipeline from being masked.
# set -o pipefail

# Start "wstpserver" in the background if `WSTP_SERVER` environment variable is set to `true`.
# The output of `wstpserver` will be redirected to stdout / stderr.
if [ "$WSTP_SERVER" = "true" ]; then
  wstpserver &
fi

MAX_TRIES=20
TRY_COUNT=0

# Wait for the WSTP server to start by trying to establish a tcp connection to it.
while true; do
  ((TRY_COUNT++))

  if ( ( echo > /dev/tcp/localhost/31415 ) &>/dev/null ); then
    break
  fi

  if [[ "$TRY_COUNT" -ge "$MAX_TRIES" ]]; then
    echo "Failed to reach wstpserver on port 31415 after $MAX_TRIES attempts."
    exit 1
  fi

  echo "Waiting for the WSTP server to start ($TRY_COUNT/$MAX_TRIES)..."
  sleep 1
done

# start the aws lambda RIE if AWS_LAMBDA_RIE is set, and AWS_LAMBDA_RUNTIME_API is not
if [ -z "${AWS_LAMBDA_RUNTIME_API}" ] && [ -n "${AWS_LAMBDA_RIE}" ]; then
  exec aws-lambda-rie "$@"
else
  exec "$@"
fi
