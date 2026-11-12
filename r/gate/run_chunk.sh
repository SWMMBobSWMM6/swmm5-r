#!/bin/bash
# run_chunk.sh NAME IDS_FILE [WAIT_SECONDS [ENGINE]] - one gate run of the R port on a chunk of the corpus:
#   nice -n 10 python3 gate/gate.py NAME "r/runswmm_saves.sh" --ids IDS -j 1 --timeout 1800 --prune
# inside flock on the shared corpus lock (waiting at most WAIT_SECONDS, default 3600, for it; ENGINE replaces
# r/runswmm_saves.sh, e.g. with a clean checkout's wrapper for the independent rerun), with the results folder and
# TMPDIR in /dev/shm (lessons/22). Afterwards the outputs of any failing model are copied to gate/results/NAME on
# disk (evidence, lessons/15) and the memory copy removed; a model stopped at the time limit is compared as far as it
# got (r/gate/stopped.py) and its partial outputs are not kept. The log is gate/NAME_run.txt; the queue, start and end
# times go to r/gate/NAME_times.txt. Never edit this file while a job is running it (lessons/41).
# (Adapted from hy/gate/run_chunk.sh.) Written by Claude (Anthropic) for Robert Dickinson; see ../README.md.
set -u
NAME=$1; IDS=$(cd "$(dirname "$2")" && pwd)/$(basename "$2"); WAIT=${3:-3600}
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
ENGINE=${4:-$ROOT/r/runswmm_saves.sh}
LOCK=${SWMM_GATE_LOCK:-${TMPDIR:-/tmp}/swmm5-gate.lock}  # one corpus run at a time
cd "$ROOT/gate"
M=/dev/shm/gate_$NAME
rm -rf "$M" && mkdir -p "$M"
[ -e "results/$NAME" ] && rm -rf "results/$NAME"
ln -s "$M" "results/$NAME"
echo "queued $(date -Is)" > "$ROOT/r/gate/${NAME}_times.txt"
TMPDIR=/dev/shm flock -w "$WAIT" "$LOCK" bash -c "echo \"started \$(date -Is)\" >> '$ROOT/r/gate/${NAME}_times.txt'; \
  nice -n 10 python3 gate.py '$NAME' '$ENGINE' --ids '$IDS' -j 1 --timeout 1800 --prune" > "${NAME}_run.txt" 2>&1
rc=$?
echo "ended $(date -Is) rc=$rc" >> "$ROOT/r/gate/${NAME}_times.txt"
# a model stopped at its time limit is not run: compare what it had written with EPA's (r/results/stopped_runs.txt)
# and keep only that line, not its partial outputs (the disk is short)
if [ -f "results/$NAME.json" ]; then
  python3 "$ROOT/r/gate/stopped.py" "results/$NAME.json" "$M"
  python3 -c "import json,sys; [print(r['id']) for r in json.load(open(sys.argv[1])) if 'TIMEOUT' in r['status']]" "results/$NAME.json" | while read -r id; do rm -rf "$M/$id"; done
  cp "results/$NAME.json" "$ROOT/r/results/" && cp "${NAME}_run.txt" "$ROOT/r/gate/"
fi
rm "results/$NAME"
mkdir -p "results/$NAME"
cp -a "$M"/. "results/$NAME"/ 2>/dev/null
rmdir "results/$NAME" 2>/dev/null
rm -rf "$M"
tail -4 "${NAME}_run.txt"
