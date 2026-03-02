#!/usr/bin/env bash
# scripts/size_report.sh — post-build code-size report for ecor_size_probe.
#
# Usage:
#   ./scripts/size_report.sh <path-to-elf>                          # sections + nm tables
#   ./scripts/size_report.sh <path-to-elf> --disasm PATTERN ...    # also disassemble matching symbols
#
# PATTERN is a case-insensitive substring matched against the fully demangled
# symbol name.  Multiple --disasm flags are accepted.
#
# Environment overrides for tool paths:
#   ARM_SIZE     (default: arm-none-eabi-size)
#   ARM_NM       (default: arm-none-eabi-nm)
#   ARM_OBJDUMP  (default: arm-none-eabi-objdump)
#
# Optional: bloaty (https://github.com/google/bloaty) for a deeper breakdown.

set -euo pipefail

ELF="${1:?Usage: $0 <path-to-elf> [--disasm PATTERN] ...}"
shift

if [[ ! -f "${ELF}" ]]; then
    echo "ERROR: ELF not found: ${ELF}" >&2
    exit 1
fi

SIZE_BIN="${ARM_SIZE:-arm-none-eabi-size}"
NM_BIN="${ARM_NM:-arm-none-eabi-nm}"
OBJDUMP_BIN="${ARM_OBJDUMP:-arm-none-eabi-objdump}"

# ── parse --disasm flags ─────────────────────────────────────────────────────
DISASM_PATTERNS=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --disasm)
            [[ $# -ge 2 ]] || { echo "ERROR: --disasm requires a PATTERN argument" >&2; exit 1; }
            DISASM_PATTERNS+=("$2")
            shift 2
            ;;
        *)
            echo "ERROR: unknown argument '$1'" >&2
            echo "Usage: $0 <elf> [--disasm PATTERN] ..." >&2
            exit 1
            ;;
    esac
done

check_tool() {
    if ! command -v "$1" &>/dev/null; then
        echo "ERROR: '$1' not found in PATH. Install arm-none-eabi binutils." >&2
        exit 1
    fi
}
check_tool "${SIZE_BIN}"
check_tool "${NM_BIN}"
if [[ ${#DISASM_PATTERNS[@]} -gt 0 ]]; then
    check_tool "${OBJDUMP_BIN}"
fi

# ── 1. Section totals ────────────────────────────────────────────────────────
echo "════════════════════════════════════════════════════════════════"
echo "  Section totals  (${ELF})"
echo "════════════════════════════════════════════════════════════════"
${SIZE_BIN} --format=sysv "${ELF}"

echo ""
echo "  berkeley summary (text + data = flash, bss = RAM zero-init):"
${SIZE_BIN} --format=berkeley "${ELF}"

# ── 2. Top symbols by size (text + rodata) ───────────────────────────────────
echo ""
echo "════════════════════════════════════════════════════════════════"
echo "  Top 40 symbols by size (text / rodata)"
echo "════════════════════════════════════════════════════════════════"
# nm output: <value> <size> <type> <name>
# Filter T/t (text) and R/r (rodata), sort descending by size, demangle.
${NM_BIN} --print-size --size-sort --radix=d --demangle "${ELF}" \
    | awk '$3 ~ /^[TtRr]$/ && $2+0 > 0 { sz=$2; $1=$2=$3=""; sub(/^ +/,""); printf "%8d  %s\n", sz, $0 }' \
    | sort -rn \
    | head -40

# ── 3. Top symbols by size (bss / data) ─────────────────────────────────────
echo ""
echo "════════════════════════════════════════════════════════════════"
echo "  Top 20 symbols by size (bss / data / rodata-data)"
echo "════════════════════════════════════════════════════════════════"
${NM_BIN} --print-size --size-sort --radix=d --demangle "${ELF}" \
    | awk '$3 ~ /^[BbDdGgSs]$/ && $2+0 > 0 { sz=$2; tp=$3; $1=$2=$3=""; sub(/^ +/,""); printf "%8d  %s  [%s]\n", sz, $0, tp }' \
    | sort -rn \
    | head -20

# ── 4. Totals by demangled namespace/prefix ──────────────────────────────────
echo ""
echo "════════════════════════════════════════════════════════════════"
echo "  Flash bytes grouped by top-level namespace / prefix"
echo "════════════════════════════════════════════════════════════════"
${NM_BIN} --print-size --size-sort --radix=d --demangle "${ELF}" \
    | awk '$3 ~ /^[TtRrDdBb]$/ && $2+0 > 0 {
        sz=$2; $1=$2=$3=""; sub(/^ +/,""); name=$0
        gsub(/<[^<>]*>/, "", name); gsub(/<[^<>]*>/, "", name)  # two passes for nested <>
        n = split(name, parts, "::")
        key = (n >= 2 ? parts[1]"::"parts[2] : parts[1])
        sum[key] += sz
      }
      END { for(k in sum) printf "%8d  %s\n", sum[k], k }' \
    | sort -rn \
    | head -30

# ── 5. Optional: bloaty ──────────────────────────────────────────────────────
echo ""
if command -v bloaty &>/dev/null; then
    echo "════════════════════════════════════════════════════════════════"
    echo "  bloaty: top compile units"
    echo "════════════════════════════════════════════════════════════════"
    bloaty --demangle=full -d compileunits "${ELF}" 2>&1 || \
        echo "  (bloaty failed — DWARF version may be unsupported, try a newer bloaty)"

    echo ""
    echo "════════════════════════════════════════════════════════════════"
    echo "  bloaty: top symbols"
    echo "════════════════════════════════════════════════════════════════"
    bloaty --demangle=full -d symbols -n 30 "${ELF}" 2>&1 || true
else
    echo "  Tip: install 'bloaty' (https://github.com/google/bloaty) for"
    echo "  a richer breakdown by compile unit, inlined function, etc."
fi

echo ""
echo "Done."

# ── 6. Disassembly of requested symbols ──────────────────────────────────────
# Called after the summary so the user sees sizes first, then the assembly.

# disasm_symbol PATTERN
#   Finds all text-section symbols whose demangled name contains PATTERN
#   (case-insensitive substring match) and prints the objdump disassembly for
#   each one, derived from the symbol's start address and byte size from nm.
disasm_symbol() {
    local pattern="$1"

    echo ""
    echo "════════════════════════════════════════════════════════════════"
    echo "  Disassembly — pattern: '${pattern}'"
    echo "════════════════════════════════════════════════════════════════"

    # nm --radix=x gives hex addresses and sizes so we can compute stop-address.
    # Format per line: ADDR(hex)  SIZE(hex)  TYPE  DEMANGLED_NAME...
    local matches
    matches=$(
        ${NM_BIN} --print-size --radix=x --demangle "${ELF}" \
            | awk '$3 ~ /^[Tt]$/ && $2+0 > 0 {
                addr=$1; sz=$2
                $1=$2=$3=""; sub(/^ +/,"")
                print addr, sz, $0
              }' \
            | grep -i "${pattern}" || true
    )

    if [[ -z "${matches}" ]]; then
        echo "  (no text symbols matching '${pattern}')"
        return
    fi

    local found=0
    while IFS= read -r line; do
        [[ -z "${line}" ]] && continue
        local addr_hex size_hex name
        addr_hex=$(awk '{print $1}' <<< "${line}")
        size_hex=$(awk '{print $2}' <<< "${line}")
        name=$(awk '{$1=$2=""; sub(/^ +/,""); print $0}' <<< "${line}")

        local addr_dec size_dec end_hex
        addr_dec=$(( 16#${addr_hex} ))
        size_dec=$(( 16#${size_hex} ))
        end_hex=$(printf "%x" $(( addr_dec + size_dec )))

        echo ""
        echo "  ── ${name}"
        printf "     0x%s .. 0x%s  (%d bytes)\n" "${addr_hex}" "${end_hex}" "${size_dec}"
        echo ""

        ${OBJDUMP_BIN} \
            --disassemble \
            --disassemble-zeroes \
            --no-show-raw-insn \
            --wide \
            --start-address="0x${addr_hex}" \
            --stop-address="0x${end_hex}" \
            "${ELF}" \
            | grep -v "^${ELF}" \
            | grep -v '^Disassembly' \
            | grep -v '^\s*$' \
            | sed 's/^/    /'

        found=$(( found + 1 ))
    done <<< "${matches}"

    echo ""
    echo "  (${found} symbol(s) matched)"
}

if [[ ${#DISASM_PATTERNS[@]} -gt 0 ]]; then
    for pat in "${DISASM_PATTERNS[@]}"; do
        disasm_symbol "${pat}"
    done
    echo ""
    echo "Disassembly done."
fi
