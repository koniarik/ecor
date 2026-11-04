#!/usr/bin/env python3
"""Compare GCC's inlining decisions between two builds of one probe.

usage: inlinediff.py BEFORE_DIR AFTER_DIR [rows]

Reads the `*.inline` dumps that `-fdump-ipa-inline-details` writes beside each object under the
two directories and prints, for every function whose number of calls or of inlined calls differs,
its calls and how many of them were inlined in each build, with the body size and the growth per
call in GCC's size units. Two phases of the IPA pass are read: the small-function inliner, and
the phase after it that inlines a function at all its calls when that removes its out-of-line
copy. At -Os they decide the calls whose inlining grows the code; the early inliner, which takes
only calls that do not grow it, has a dump of its own, so a function with no calls in one build
was inlined early there or is not called at all. Functions are named by their template, without
template arguments.

A refusal "call is unlikely and code size would grow" whose callee GCC itself estimated not to
grow the file when inlined at all its calls is counted apart: the quick pre-check in GCC's
`growth_positive_p` counts each call it has already estimated at its inlined size instead of its
growth, which caps the first phase at about two calls per file for a small function; only the
second phase, with its limit of 4000 units on every caller, takes more.
"""
import collections
import re
import sys
from pathlib import Path

WITH = re.compile(r' \[with [^\]]*\]')
CONSIDERING = re.compile(r'^Considering (.*)/\d+ with (-?\d+) size$')
INTO = re.compile(r'^ to be inlined into (.*)/(\d+) in .*:(-?\d+)$')
GROWTH = re.compile(r'size growth (-?\d+)')
OVERALL = re.compile(r'overall growth (-?\d+) \(current\)')
INLINED = re.compile(r'optimized:  Inlined ')
MISSED = re.compile(r'missed:   not inlinable: (.*)/(\d+) -> (.*)/\d+, (.*)$')
TO_ALL = re.compile(r'^Inlining (.*)/\d+ size (-?\d+)\.$')
UNLIKELY = 'call is unlikely and code size would grow'


def name_of(signature):
    """The qualified function name of a dumped signature, without return type or arguments."""
    s = WITH.sub('', signature).replace('(anonymous namespace)', '{anonymous}')
    depth, start, i = 0, 0, 0
    while i < len(s):
        if s.startswith('operator', i):
            i += len('operator')
            while i < len(s) and s[i] != '(':
                i += 1
            if s.startswith('()', i):
                i += 2
            while i < len(s) and s[i] not in ' (':
                i += 1
            return s[start:i]
        c = s[i]
        if c == '<':
            depth += 1
        elif c == '>':
            depth -= 1
        elif c == ' ' and depth == 0:
            start = i + 1
        elif c == '(' and depth == 0:
            return s[start:i]
        i += 1
    return s[start:]


def decisions(path):
    """One dict per call the small-function inliner decided: callee, size, growth, overall
    growth, inlined. Calls refused before the heap carry no figures. A call that the later phase
    inlines was refused once already and has `again` set."""
    out, cur, in_heap = [], None, False
    for line in open(path, errors='replace'):
        line = line.rstrip('\n')
        m = TO_ALL.match(line)
        if m:
            out.append({'callee': name_of(m.group(1)), 'size': int(m.group(2)), 'growth': None,
                        'overall': None, 'site': None, 'inlined': True, 'reason': None,
                        'again': True})
            continue
        m = CONSIDERING.match(line)
        if m:
            in_heap = True
            cur = {'callee': name_of(m.group(1)), 'size': int(m.group(2)), 'growth': None,
                   'overall': None, 'site': None}
            continue
        if line.startswith('Unit growth for small function inlining'):
            in_heap, cur = False, None
            continue
        if cur is not None:
            m = INTO.match(line)
            if m:
                cur['site'] = (m.group(2), m.group(3))
                continue
            m = GROWTH.search(line)
            if m and cur['growth'] is None:
                cur['growth'] = int(m.group(1))
            m = OVERALL.search(line)
            if m and cur['overall'] is None:
                cur['overall'] = int(m.group(1))
            if INLINED.search(line):
                out.append(dict(cur, inlined=True, reason=None))
                cur = None
                continue
            m = MISSED.search(line)
            if m:
                out.append(dict(cur, inlined=False, reason=m.group(4)))
                cur = None
            continue
        m = MISSED.search(line)
        if m and not in_heap:
            out.append({'callee': name_of(m.group(3)), 'size': None, 'growth': None,
                        'overall': None, 'site': None, 'inlined': False, 'reason': m.group(4)})
    return out


def summary(directory):
    """Per callee: calls, inlined calls, (size, growth) pairs; and the count of refusals that
    contradict GCC's own whole-file estimate."""
    calls, inlined = collections.Counter(), collections.Counter()
    shapes = collections.defaultdict(collections.Counter)
    contradicted = 0
    for dump in sorted(Path(directory).rglob('*.inline')):
        seen = set()
        for d in decisions(dump):
            if d.get('again'):
                inlined[d['callee']] += 1
                calls[d['callee']] = max(calls[d['callee']], inlined[d['callee']])
                continue
            if d['site'] is not None:
                key = (d['callee'], d['site'])
                if key in seen:
                    continue
                seen.add(key)
                shapes[d['callee']][(d['size'], d['growth'])] += 1
            calls[d['callee']] += 1
            inlined[d['callee']] += d['inlined']
            if d['reason'] == UNLIKELY and d['overall'] is not None and d['overall'] <= 0:
                contradicted += 1
    return calls, inlined, shapes, contradicted


def shape_text(shapes):
    return ', '.join(f'{s}/{g}' for (s, g), _ in shapes.most_common(2)) or '-'


def inlinediff(before, after, rows=25, width=110):
    bc, bi, bs, bx = summary(before)
    ac, ai, as_, ax = summary(after)
    print(f'inlined calls {sum(bi.values())} -> {sum(ai.values())}; refused although GCC '
          f'estimated no growth: {bx} -> {ax}')
    changed = sorted((k for k in set(bc) | set(ac) if bi[k] != ai[k] or bc[k] != ac[k]),
                     key=lambda k: (-abs(ai[k] - bi[k]), -abs(ac[k] - bc[k])))
    if not changed:
        return
    print('   inlined of calls          size/growth          function')
    for k in changed[:rows]:
        cells = f'{bi[k]:5} of {bc[k]:5} -> {ai[k]:5} of {ac[k]:5}'
        shapes = f'{shape_text(bs[k])} -> {shape_text(as_[k])}'
        print(f'{cells}  {shapes:20} {k[:width]}')
    if len(changed) > rows:
        print(f'... {len(changed) - rows} more')


if __name__ == '__main__':
    inlinediff(sys.argv[1], sys.argv[2], int(sys.argv[3]) if len(sys.argv) > 3 else 25)
