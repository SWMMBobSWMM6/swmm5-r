#!/usr/bin/env python3
"""devrun.py [--timeout S] [--engine RUNSWMM] ID... - development runs of the R port: each deck is copied alone to /dev/shm/r_dev/ID,
run there with r/runswmm, and compared with EPA 5.2.4's outputs in gate/ref_outputs the way gate/gate.py compares.
Decks that read other files may fail here for that reason only. Written by Claude (Anthropic)."""
import os, re, shutil, subprocess, sys, time
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
TS = re.compile(rb'Analysis begun|Analysis ended|Total elapsed time')
paths = dict(l.rstrip('\n').split('\t') for l in open(os.path.join(ROOT, 'gate/models_all.tsv')))
args = sys.argv[1:]
to = 600
eng = os.path.join(ROOT, 'r/runswmm')
while args and args[0] in ('--timeout', '--engine'):
    if args[0] == '--timeout': to = float(args[1])
    else: eng = args[1]
    args = args[2:]
for mid in args:
    src = paths[mid]
    d = '/dev/shm/r_dev/' + mid
    shutil.rmtree(d, ignore_errors=True); os.makedirs(d)
    inp = os.path.join(d, os.path.basename(src)); shutil.copy(src, inp)
    t0 = time.time()
    try:
        p = subprocess.run([eng, os.path.basename(src), 'r.rpt', 'r.out'], cwd=d,
                           capture_output=True, timeout=to)
        rc = p.returncode; err = p.stderr.decode(errors='replace')
    except subprocess.TimeoutExpired:
        rc = 'timeout'; err = ''
    sec = time.time() - t0
    ref = os.path.join(ROOT, 'gate/ref_outputs', mid)
    def lines(f):
        try: return [l for l in open(f, 'rb').read().split(b'\n') if not TS.search(l)]
        except FileNotFoundError: return None
    a, b = lines(ref + '/r.rpt'), lines(d + '/r.rpt')
    st = []
    if rc != 0: st.append('CRASH(%s)' % rc)
    if a != b: st.append('RPTDIFF')
    ro = ref + '/r.out'
    if os.path.exists(ro) and os.path.getsize(ro) > 0:
        if not os.path.exists(d + '/r.out') or open(ro, 'rb').read() != open(d + '/r.out', 'rb').read(): st.append('OUTDIFF')
    elif os.path.exists(d + '/r.out') and os.path.getsize(d + '/r.out') > 0: st.append('OUTDIFF')
    print('%s %-8s %7.1fs %s' % (mid, '+'.join(st) or 'PASS', sec, os.path.relpath(src, '/home/claude/swmmbobswmm6/1729-swmm5-models-2030')), flush=True)
    if st and a is not None and b is not None and a != b:
        for i, (x, y) in enumerate(zip(a, b)):
            if x != y:
                print('   line %d\n   EPA: %s\n   R:   %s' % (i, x.decode(errors='replace')[:150], y.decode(errors='replace')[:150])); break
        else:
            print('   lengths %d vs %d' % (len(a), len(b)))
    if rc != 0 and err:
        print('   ' + err.strip()[-1500:].replace('\n', '\n   '))
    if not st:
        shutil.rmtree(d, ignore_errors=True)
