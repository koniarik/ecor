#!/usr/bin/env python3
"""Code-size suite: generated probes compiled for Cortex-M4 with arm-none-eabi-g++ at -Os.

usage:
  size.py [run] [options]            build, measure, report, compare with the baseline
  size.py update-baseline [options]  as run, then write the baseline for this toolchain
  size.py explain PROBE [options]    section-level diff of one probe between header variants;
                                     PROBE is a label as the report prints it, family/key

options:
  --quick | --full          the probe tier; quick is the default and a subset of full
  --family NAME             only this family, repeatable
  --against REF             also build every probe with REF's header and report cur − REF
  --variant FILE:NAME       also build with the header that `python3 FILE SRC OUT NAME` writes
                            to OUT/ecor/ecor.hpp
  --build-dir DIR           default test/size/build
  --tolerance PERCENT       regression threshold against the baseline, default 1, with a floor
                            of 8 bytes
  --no-baseline             do not compare with the baseline
  --fail-on-growth          with --against: exit with 1 when a probe grew past the tolerance
                            against REF's header, which needs no baseline for the toolchain
  --extra-flags FLAGS       appended to every compile, for instance -fopt-info-inline-all

A probe's figure is the sum of its `.text*` and `.rodata*` sections from `size -A`; `.data*` and
`.bss*` are recorded beside it. The report goes to BUILD/report.md and the raw figures to
BUILD/results.json. The exit status is 1 when a probe exceeds the baseline by more than the
tolerance or when the build fails.
"""
import argparse
import json
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
sys.path.insert(0, str(HERE))

from scenarios.common import CPU, TIERS, Measurement  # noqa: E402
from scenarios.registry import FAMILIES  # noqa: E402

CXX, SIZE = 'arm-none-eabi-g++', 'arm-none-eabi-size'


def run(cmd, **kw):
    print('+', ' '.join(str(c) for c in cmd), flush=True)
    return subprocess.run([str(c) for c in cmd], check=True, **kw)


def toolchain_id():
    v = subprocess.run([CXX, '-dumpfullversion'], capture_output=True, text=True, check=True)
    return f'arm-none-eabi-gcc-{v.stdout.strip()}'


def write_if_changed(path, text):
    path.parent.mkdir(parents=True, exist_ok=True)
    if not path.exists() or path.read_text() != text:
        path.write_text(text)


def header_dirs(args, build):
    """Header variant name to include directory; 'cur' is the working tree."""
    hdrs = {'cur': REPO / 'include'}
    if args.against:
        d = build / 'hdr' / 'ref'
        out = subprocess.run(['git', '-C', REPO, 'show', f'{args.against}:include/ecor/ecor.hpp'],
                             capture_output=True, text=True, check=True).stdout
        write_if_changed(d / 'ecor' / 'ecor.hpp', out)
        hdrs['ref'] = d
    if args.variant:
        script, name = args.variant.rsplit(':', 1)
        d = build / 'hdr' / name
        d.mkdir(parents=True, exist_ok=True)
        run([sys.executable, script, REPO / 'include' / 'ecor' / 'ecor.hpp', d, name])
        hdrs[name] = d
    return hdrs


def collect(args):
    probes = []
    for fam in FAMILIES:
        if args.family and fam.NAME not in args.family:
            continue
        probes += fam.probes(args.tier)
    return probes


def generate(probes, hdrs, build):
    """Writes the probe sources and the CMake matrix; returns one manifest row per target."""
    lines, manifest = [], []
    for i, p in enumerate(probes):
        if p.is_image:
            d = build / 'src' / p.family / f'{i:04d}'
            files = []
            for name, text in p.sources():
                write_if_changed(d / name, text)
                files.append((d / name).as_posix())
        else:
            src = build / 'src' / p.family / f'{i:04d}.cpp'
            write_if_changed(src, p.source())
        for h, hdir in hdrs.items():
            target = f'{p.family}_{i:04d}_{h}'
            if p.is_image:
                lines.append(f'size_image({target} {hdir.as_posix()} {CPU} {int(p.lto)} '
                             + ' '.join(files) + ')')
                artifact = (build / 'images' / f'{target}.elf').as_posix()
            else:
                defs = ' '.join(f'{k}={v}' for k, v in p.defines.items())
                lines.append(f'size_probe({target} {src.as_posix()} {hdir.as_posix()} {CPU} {defs})')
                artifact = None
            manifest.append({'target': target, 'header': h, 'family': p.family, 'key': p.key,
                             'artifact': artifact})
    write_if_changed(build / 'matrix.cmake', '\n'.join(lines) + '\n')
    return manifest


def build_all(build, args):
    cmd = ['cmake', '-S', HERE, '-B', build, '-G', 'Ninja',
           '--toolchain', HERE / 'arm-none-eabi.cmake',
           f'-DMATRIX={(build / "matrix.cmake").as_posix()}',
           f'-DECOR_SIZE_EXTRA_FLAGS={args.extra_flags or ""}']
    local_zll = sorted(REPO.glob('_build/*/_deps/zll-src/include/zll.hpp'))
    if local_zll:
        cmd.append(f'-DFETCHCONTENT_SOURCE_DIR_ZLL={local_zll[0].parent.parent}')
    run(cmd)
    run(['cmake', '--build', build])


def sections(path):
    """(.text + .rodata, .data + .bss) of one object or ELF."""
    out = subprocess.run([SIZE, '-A', path], capture_output=True, text=True, check=True).stdout
    flash = ram = 0
    for line in out.splitlines():
        p = line.split()
        if len(p) >= 2 and p[1].isdigit():
            if p[0].startswith(('.text', '.rodata')):
                flash += int(p[1])
            elif p[0].startswith(('.data', '.bss')):
                ram += int(p[1])
    return flash, ram


def objects_of(build, target):
    d = build / 'CMakeFiles' / f'{target}.dir'
    return [o for o in d.rglob('*') if o.suffix in ('.o', '.obj')]


def measure(build, manifest):
    results = {}
    for m in manifest:
        if m['artifact']:
            paths = [Path(m['artifact'])]
        else:
            paths = objects_of(build, m['target'])
        flash = ram = 0
        for o in paths:
            f, r = sections(o)
            flash += f
            ram += r
        results[(m['header'], m['family'], m['key'])] = Measurement(flash, ram, str(paths[0]))
    return results


def baseline_path(tid):
    return HERE / 'baseline' / f'{tid}.json'


def load_baseline(tid):
    p = baseline_path(tid)
    return json.loads(p.read_text()) if p.exists() else None


def compare(results, base, tolerance):
    """Rows (label, current, baseline, delta, over) for every probe in both. A family's
    TOLERANCE, (percent, floor in bytes), replaces the command line's percent and 8-byte floor."""
    tolerances = {f.NAME: getattr(f, 'TOLERANCE', (tolerance, 8)) for f in FAMILIES}
    rows = []
    for (h, fam, key), m in sorted(results.items()):
        if h != 'cur':
            continue
        b = base['results'].get(f'{fam}/{key}')
        if b is None:
            continue
        pct, floor = tolerances[fam]
        limit = max(floor, b[0] * pct / 100)
        rows.append((f'{fam}/{key}', m.flash, b[0], m.flash - b[0], m.flash - b[0] > limit))
    return rows


def against_as_baseline(results, header):
    """The figures of header variant `header` in the baseline's shape, so `compare` can judge
    the working tree against a commit's header built in the same run."""
    return {'results': {f'{fam}/{key}': [m.flash, m.ram]
                        for (h, fam, key), m in results.items() if h == header}}


def write_report(build, results, hdrs, args, tid, base):
    L = []
    w = L.append
    w('# ecor size report')
    w('')
    w(f'{CPU}, toolchain {tid}, tier {args.tier}, header variants {", ".join(hdrs)}.'
      + (f' Baseline {baseline_path(tid).name}.' if base else ' No baseline for this toolchain.'))
    w('')
    headers = list(hdrs)
    for fam in FAMILIES:
        fam_results = {(h, k): m for (h, f, k), m in results.items() if f == fam.NAME}
        if not fam_results:
            continue
        w(f'## {fam.NAME}')
        w('')
        fam.report(fam_results, headers, w)
    comparisons = []
    if base:
        comparisons.append(('the baseline', compare(results, base, args.tolerance)))
    if args.against:
        comparisons.append((f'{args.against}', compare(results, against_as_baseline(results, 'ref'),
                                                         args.tolerance)))
    for what, rows in comparisons:
        worse = [r for r in rows if r[4]]
        w(f'## against {what}')
        w('')
        w(f'{len(rows)} probes compared, {len(worse)} above the tolerance of {args.tolerance}%'
          ' or 8 bytes.')
        w('')
        changed = [r for r in rows if r[3]]
        if changed:
            w(f'| probe | {what} | now | delta |')
            w('|---|---:|---:|---:|')
            for label, now, b, d, over in sorted(changed, key=lambda r: -abs(r[3])):
                w(f'| {label} | {b} | {now} | {d:+d}{" **over**" if over else ""} |')
            w('')
    w('## every probe')
    w('')
    w('| probe | ' + ' | '.join(f'{h} flash' for h in headers) + ' | cur ram |')
    w('|---|' + '---:|' * (len(headers) + 1))
    for (h, fam, key), m in sorted(results.items()):
        if h != 'cur':
            continue
        others = [results.get((o, fam, key)) for o in headers[1:]]
        cells = [str(m.flash)] + ['-' if o is None else f'{o.flash} ({m.flash - o.flash:+d})'
                                  for o in others]
        w(f'| {fam}/{key} | ' + ' | '.join(cells) + f' | {m.ram} |')
    text = '\n'.join(L) + '\n'
    (build / 'report.md').write_text(text)
    return text


def save_results(build, results, tid, args):
    data = {'toolchain': tid, 'cpu': CPU, 'tier': args.tier,
            'results': {f'{fam}/{key}': [m.flash, m.ram]
                        for (h, fam, key), m in sorted(results.items()) if h == 'cur'}}
    (build / 'results.json').write_text(json.dumps(data, indent=1) + '\n')
    return data


def explain(args, build, manifest):
    """Section diff of one probe between 'cur' and the other header variants."""
    sys.path.insert(0, str(HERE / 'tools'))
    from secdiff import secdiff  # noqa: E402
    rows = [m for m in manifest if f"{m['family']}/{m['key']}" == args.probe]
    cur = [m for m in rows if m['header'] == 'cur']
    if not cur:
        sys.exit(f'no probe {args.probe}; labels are family/key as the report prints them')
    def artifact(m):
        return Path(m['artifact']) if m['artifact'] else objects_of(build, m['target'])[0]

    for other in [m for m in rows if m['header'] != 'cur']:
        print(f'=== {args.probe}: {other["header"]} -> cur ===')
        secdiff(artifact(other), artifact(cur[0]))


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('command', nargs='?', default='run',
                    choices=['run', 'update-baseline', 'explain'])
    ap.add_argument('probe', nargs='?', help='for explain: the probe label, family/key')
    tier = ap.add_mutually_exclusive_group()
    tier.add_argument('--quick', dest='tier', action='store_const', const='quick', default='quick')
    tier.add_argument('--full', dest='tier', action='store_const', const='full')
    ap.add_argument('--family', action='append')
    ap.add_argument('--against')
    ap.add_argument('--variant')
    ap.add_argument('--build-dir', type=Path, default=HERE / 'build')
    ap.add_argument('--tolerance', type=float, default=1.0)
    ap.add_argument('--no-baseline', action='store_true')
    ap.add_argument('--fail-on-growth', action='store_true')
    ap.add_argument('--extra-flags')
    args = ap.parse_args()
    assert args.tier in TIERS

    build = args.build_dir.resolve()
    build.mkdir(parents=True, exist_ok=True)
    tid = toolchain_id()
    hdrs = header_dirs(args, build)
    probes = collect(args)
    if args.command == 'explain':
        if not args.probe:
            sys.exit('explain needs a probe label')
        probes = [p for p in probes if p.label == args.probe]
        if not probes:
            sys.exit(f'no probe {args.probe} in tier {args.tier}')
        if len(hdrs) == 1:
            sys.exit('explain needs --against REF or --variant FILE:NAME')
    manifest = generate(probes, hdrs, build)
    build_all(build, args)
    if args.command == 'explain':
        explain(args, build, manifest)
        return
    results = measure(build, manifest)
    base = None if args.no_baseline else load_baseline(tid)
    text = write_report(build, results, hdrs, args, tid, base)
    data = save_results(build, results, tid, args)
    if args.command == 'update-baseline':
        p = baseline_path(tid)
        p.parent.mkdir(exist_ok=True)
        p.write_text(json.dumps(data, indent=1) + '\n')
        print(f'baseline written: {p}')
        return
    print(text)
    worse = []
    if base:
        worse += [r for r in compare(results, base, args.tolerance) if r[4]]
    if args.against and args.fail_on_growth:
        worse += [r for r in compare(results, against_as_baseline(results, 'ref'), args.tolerance)
                  if r[4]]
    if worse:
        print(f'{len(worse)} probes above the tolerance; see {build / "report.md"}')
        sys.exit(1)


if __name__ == '__main__':
    main()
