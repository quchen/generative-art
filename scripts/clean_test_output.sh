#!/usr/bin/env bash

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
for svg in $(find test/out -name "*.svg"); do
    rm "$svg"
done
