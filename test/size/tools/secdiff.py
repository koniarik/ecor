#!/usr/bin/env python3
"""Attribute a size difference between two object files to sections.

usage: secdiff.py BEFORE.obj AFTER.obj [name-width]

Sums `.text*` and `.rodata*` sections from `size -A`, the figure the reports use, and prints
every section whose size differs, largest absolute delta first, with the demangled name when
the section is named after a function. A section diff counts ICF-folded aliases once, where a
symbol diff over `nm` counts every alias in full.
"""
import subprocess
import sys


def sections(path):
    out = subprocess.run(['arm-none-eabi-size', '-A', str(path)], capture_output=True, text=True,
                         check=True).stdout
    d = {}
    for line in out.splitlines():
        p = line.split()
        if len(p) >= 2 and p[1].isdigit() and p[0].startswith(('.text', '.rodata')):
            d[p[0]] = int(p[1])
    return d


def demangle(names):
    mangled = [n.split('.', 2)[2] if n.count('.') >= 2 else n for n in names]
    out = subprocess.run(['arm-none-eabi-c++filt'], input='\n'.join(mangled),
                         capture_output=True, text=True, check=True).stdout.splitlines()
    return dict(zip(names, out))


def secdiff(before, after, width=150):
    a, b = sections(before), sections(after)
    print(f'total {sum(a.values())} -> {sum(b.values())} ({sum(b.values()) - sum(a.values()):+d})')
    rows = sorted(((b.get(k, 0) - a.get(k, 0), a.get(k, 0), b.get(k, 0), k)
                   for k in set(a) | set(b) if a.get(k, 0) != b.get(k, 0)),
                  key=lambda r: -abs(r[0]))
    names = demangle([r[3] for r in rows])
    for d, x, y, k in rows:
        print(f'{d:+5d} {x:5d} -> {y:5d}  {names[k][:width]}')


if __name__ == '__main__':
    secdiff(sys.argv[1], sys.argv[2], int(sys.argv[3]) if len(sys.argv) > 3 else 150)
