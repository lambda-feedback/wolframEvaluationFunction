#!/bin/bash
# Fetches the pinned toolkit-wolfram version (see .toolkit-wolfram-version)
# into ./toolkit-wolfram, for running/testing this evaluation function
# locally without Docker/podman. Re-run any time to reset to the pinned
# version (e.g. after bumping .toolkit-wolfram-version).
#
# Usage:
#   scripts/setup-toolkit.sh
#
# Then, to run the evaluation function directly:
#   export LF_TOOLKIT_PATH=./toolkit-wolfram
#   wolframscript -f ./toolkit-wolfram/Bootstrap.wl request.json response.json

set -euo pipefail

cd "$(dirname "$0")/.."

VERSION=$(cat .toolkit-wolfram-version)
DEST="toolkit-wolfram"

rm -rf "$DEST"
git clone --branch "$VERSION" --depth 1 \
  https://github.com/lambda-feedback/toolkit-wolfram.git "$DEST"

echo "Fetched toolkit-wolfram $VERSION -> $DEST"
