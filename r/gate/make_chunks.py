#!/usr/bin/env python3
"""make_chunks.py PREFIX RATIO LIMIT [IDS] - cut the gate models (or those in IDS) into chunks for background jobs that
must finish, queue included, inside the sandbox's two-hour limit. A model's time is estimated as RATIO times the Hy
port's gate time (hy/results/hy_1*.json; hy_long2_72hh.json for EPA/72_H&H_Elements(2).inp) plus 1 s of start-up,
capped at the gate's 1800 s limit; models are taken in corpus order and a chunk is closed before its estimate would
pass LIMIT seconds. Writes r/gate/PREFIX<letter>.txt and prints each chunk's size and estimate.
Written by Claude (Anthropic) for Robert Dickinson; see ../README.md."""
import glob, json, os, sys
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
prefix, ratio, limit = sys.argv[1], float(sys.argv[2]), float(sys.argv[3])
t = {}
for f in sorted(glob.glob(os.path.join(ROOT, 'hy/results/hy_1*.json'))) + glob.glob(os.path.join(ROOT, 'hy/results/hy_long2_72hh.json')):
    for r in json.load(open(f)):
        t[r['id']] = r['sec']
ids = [l.split('\t')[0] for l in open(os.path.join(ROOT, 'gate/models_all.tsv'))]
if len(sys.argv) > 4:
    keep = set(open(sys.argv[4]).read().split())
    ids = [i for i in ids if i in keep]
est = {i: min(1805.0, ratio * t.get(i, 60.0) + 1.0) for i in ids}
chunks, cur, s = [], [], 0.0
for i in ids:
    if cur and s + est[i] > limit:
        chunks.append((cur, s)); cur, s = [], 0.0
    cur.append(i); s += est[i]
if cur:
    chunks.append((cur, s))
for k, (c, s) in enumerate(chunks):
    name = '%s%s.txt' % (prefix, 'abcdefghijklmnopqrstuvwxyz'[k] if k < 26 else 'z' + 'abcdefghijklmnopqrstuvwxyz'[k - 26])
    open(os.path.join(ROOT, 'r/gate', name), 'w').write('\n'.join(c) + '\n')
    print('%s %3d models, estimate %5.0f s' % (name, len(c), s))
print('total %d models, estimate %.1f h' % (sum(len(c) for c, s in chunks), sum(s for c, s in chunks) / 3600))
