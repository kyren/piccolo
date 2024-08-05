#!/usr/bin/env bash

set -euo pipefail
cd "$(dirname "$0")"

LUA_VERSION=$(lua -v)

if [[ "$LUA_VERSION" != "Lua 5.4"* || "$LUA_VERSION" != *"PUC-Rio"* ]]; then
    echo "Lua version not PUC-Rio Lua 5.4"
    exit 1
fi

TEST_FAIL=

for test_dir in ./scripts ./scripts-wishlist; do
    for file in "$test_dir"/*; do
        if ! lua "$file"; then
            TEST_FAIL=1
        fi
    done
done

if [ -n "$TEST_FAIL" ]; then 
    echo "some tests failed in PUC-Rio Lua 5.4!"
    exit 1
fi
