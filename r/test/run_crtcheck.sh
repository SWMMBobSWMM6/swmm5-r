#!/bin/sh
# run_crtcheck.sh - check the R runtime's printf, strtod and sscanf against the system glibc: tools/crt/test/mkfmt.c
# writes about 1.4 million reference cases from glibc; test/crtcheck.R replays them through swmm5/rt.R. The cases
# go to a temporary folder (TMPDIR) and are deleted. Written by Claude (Anthropic) for Robert Dickinson.
set -e
cd "$(dirname "$0")/.."
T=$(mktemp -d "${TMPDIR:-/tmp}/rcrt.XXXXXX")
gcc -O2 -o "$T/mkfmt" ../tools/crt/test/mkfmt.c -lm
"$T/mkfmt" > "$T/fmt.txt"
Rscript --vanilla test/crtcheck.R "$PWD/swmm5" "$T/fmt.txt"
rm -rf "$T"
