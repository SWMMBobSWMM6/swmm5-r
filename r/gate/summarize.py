#!/usr/bin/env python3
"""summarize.py - the R port's gate result: each model's last record (runs are read in the order below, so a later
run of a model replaces an earlier one), the counts, every model that did not pass with its first difference, and
the time the runs took (sum of the gate's per-model seconds, and of each run's start-to-end time).
Written by Claude (Anthropic) for Robert Dickinson."""
import json, os, re, sys, datetime
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
R = os.path.join(ROOT, 'r')
# the full gate: engine de46ab3 for r_d1 to r_2t, b9dbf76 for r_2u and r_3a, af6b367 from r_2va on; r_4a-r_4d run
# the RDII decks again on af6b367 and replace their earlier records
ORDER = ['r_d1', 'r_1a'] + ['r_2' + c for c in 'abcdefghijklmnopqrstu'] + ['r_3a', 'r_2va', 'r_2vb', 'r_4a', 'r_4b',
         'r_4c', 'r_4d'] + ['r_2' + c for c in ('w', 'x', 'y', 'z', 'za', 'bb', 'ba')]
last, used, secs, wall = {}, [], 0.0, 0.0
for name in ORDER:
    p = os.path.join(R, 'results', name + '.json')
    if not os.path.exists(p):
        continue
    used.append(name)
    for r in json.load(open(p)):
        last[r['id']] = dict(r, run=name)
    t = open(os.path.join(R, 'gate', name + '_times.txt')).read()
    s, e = re.findall(r'started (\S+)', t), re.findall(r'ended (\S+)', t)
    if s and e:
        wall += (datetime.datetime.fromisoformat(e[-1]) - datetime.datetime.fromisoformat(s[-1])).total_seconds()
for r in last.values():
    secs += r['sec']
counts = {}
for r in last.values():
    counts[r['status']] = counts.get(r['status'], 0) + 1
print('runs:', ' '.join(used))
print('models: %d  %s' % (len(last), ', '.join('%s %d' % kv for kv in sorted(counts.items(), key=lambda kv: -kv[1]))))
print('model seconds: %.0f (%.1f h); run start-to-end: %.0f s (%.1f h)' % (secs, secs / 3600, wall, wall / 3600))
for r in sorted(last.values(), key=lambda r: r['path']):
    if r['status'] != 'PASS':
        print('%-26s %s %s  %s' % (r['status'], r['run'], r['id'], r['path'].split('2030/')[-1]), r['first_rpt_diff'])
if len(sys.argv) > 1:
    open(sys.argv[1], 'w').write(''.join(i + '\n' for i in sorted(i for i, r in last.items() if r['status'] == 'PASS')))
