#!/usr/bin/env python3
"""stopped.py RUN_JSON RESULTS_DIR - for each model a gate run stopped at its time limit, compare what the engine had
written with EPA's reference outputs (the .out byte for byte up to its length, the .rpt line for line up to its last
complete line, clock lines excluded) and append one line per model to r/results/stopped_runs.txt. A stopped model is
counted as not run whatever this shows. Written by Claude (Anthropic) for Robert Dickinson; see ../README.md."""
import json, os, re, sys
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
TS = re.compile(rb'Analysis begun|Analysis ended|Total elapsed time')
run, res = sys.argv[1], sys.argv[2]
name = os.path.basename(run)[:-5]
out = open(os.path.join(ROOT, 'r/results/stopped_runs.txt'), 'a')
for r in json.load(open(run)):
    if 'TIMEOUT' not in r['status']:
        continue
    d = os.path.join(res, r['id']); ref = os.path.join(ROOT, 'gate/ref_outputs', r['id'])
    msg = []
    try:
        a = open(os.path.join(d, 'r.out'), 'rb').read(); b = open(os.path.join(ref, 'r.out'), 'rb').read()
        same = b[:len(a)] == a
        msg.append('.out %d of %d bytes written, %s' % (len(a), len(b), 'identical to EPA\'s so far' if same else 'DIFFERENT from EPA\'s'))
    except FileNotFoundError:
        msg.append('.out not written')
    try:
        a = open(os.path.join(d, 'r.rpt'), 'rb').read().split(b'\n')[:-1]; b = open(os.path.join(ref, 'r.rpt'), 'rb').read().split(b'\n')
        a = [l for l in a if not TS.search(l)]; b = [l for l in b if not TS.search(l)]
        msg.append('.rpt %d of %d lines written, %s' % (len(a), len(b), 'identical so far' if b[:len(a)] == a else 'DIFFERENT'))
    except FileNotFoundError:
        msg.append('.rpt not written')
    out.write('%s  %s  %s  stopped after %.0f s: %s\n' % (name, r['id'], os.path.relpath(r['path'], '/home/claude/swmmbobswmm6/1729-swmm5-models-2030'), r['sec'], '; '.join(msg)))
    print(name, r['id'], '; '.join(msg))
