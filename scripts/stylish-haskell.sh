#!/usr/bin/env bash

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"
for sourceFile in $(git ls-files "*.hs"); do
    stylish-haskell -i "$sourceFile"
done
