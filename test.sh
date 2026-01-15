#!/usr/bin/env bash
set -e

ARGS=()

ARGS+=(test)
ARGS+=(--test test)
# following args are ignored by the cargo and forwarded to test harness
ARGS+=(--)
# don't capture stdout & stderr, but also allows for the following two args to work
ARGS+=(--nocapture)
# test name filter, otherwise my params are interpreted as filters
ARGS+=("")
# following args are ignored by the test harness and forwarded to actual test code
ARGS+=(--)
# actual user args
ARGS+=("$@")

echo cargo "${ARGS[@]}"
cargo "${ARGS[@]}"
