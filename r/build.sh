#!/bin/sh
# build.sh - byte-compile the R port's two engines into swmm5/engine_fast.rds and swmm5/engine_safe.rds
# (about 40 seconds); runswmm loads them.
set -e
H=$(cd "$(dirname "$0")" && pwd)
Rscript --vanilla "$H/swmm5/build.R" "$H/swmm5"
