#!/bin/bash
# run_libm_tests.sh [N] - check the math the R engine uses against the system libm (the library EPA's runswmm calls),
# bit for bit: N random arguments per function over SWMM's ranges plus special values (tools/libm/test/mkvectors.c),
# an edge-case set (mkedge.c) and the exact functions and C casts (test/mkexact.c), all replayed by test/libmcheck.R.
# The vectors (about 260 MB at N = 1000000) go to R_LIBM_WORK (default /dev/shm/r_libm_test) and are deleted.
# Written by Claude (Anthropic) for Robert Dickinson; see ../README.md.
set -e
HERE=$(cd "$(dirname "$0")" && pwd); P=$(cd "$HERE/.." && pwd); ROOT=$(cd "$P/.." && pwd)
N=${1:-1000000}; W=${R_LIBM_WORK:-/dev/shm/r_libm_test}; rm -rf "$W"; mkdir -p "$W"
gcc -O2 -o "$W/mkvectors" "$ROOT/tools/libm/test/mkvectors.c" -lm
gcc -O2 -o "$W/mkedge" "$ROOT/tools/libm/test/mkedge.c" -lm
gcc -O2 -fno-builtin -o "$W/mkexact" "$HERE/mkexact.c" -lm
"$W/mkvectors" "$N" "$W/vectors.bin" > /dev/null 2>&1
"$W/mkedge" 100000 "$W/edge.bin" > /dev/null 2>&1
"$W/mkexact" "$N" "$W/exact.bin" > /dev/null 2>&1
echo "== $(R --version | sed -n 1p), N = $N per function"
Rscript --vanilla "$HERE/libmcheck.R" "$P/swmm5" "$W/vectors.bin" "$W/edge.bin" "$W/exact.bin"
rm -rf "$W"
