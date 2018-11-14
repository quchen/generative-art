#!/usr/bin/env bash

set -euo pipefail
echo "Normalizing SVG so output is deterministic and doesn’t confuse Git"
cd "$(git rev-parse --show-toplevel)"
for image in test/out/*.svg; do
    ID="$(basename -s .svg "$image")"
    perl -pi -e "s/^<g id=\"surface(\\d+)\">\$/<g id=\"$ID\">/" "$image"
done
