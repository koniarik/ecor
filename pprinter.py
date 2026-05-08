import re
import struct

import gdb
import gdb.printing

# ---------------------------------------------------------------------------
# ecor::_promise_type<Task>  pretty printer
# ---------------------------------------------------------------------------
#
# Coroutine frame layout (GCC):
#
#   offset  0           : resume_fn*   (ptr_size bytes)
#   offset  ptr_size    : destroy_fn*  (ptr_size bytes)
#   offset  2*ptr_size  : promise object  ← we have this address
#
# Verified on GCC 15.2:
#   aarch64 / x86-64 (ptr_size=8) → promise at frame+16
#   arm-none-eabi arm32 (ptr_size=4) → promise at frame+8
#
# So:  PROMISE_OFFSET = 2 * ptr_size  (derived at runtime from the inferior)


def _resume_fn_addr(promise_val):
    """Return the integer address of the coroutine resume function given a
    gdb.Value for the promise object.  Returns None on any error.
    """
    try:
        ptr_size = gdb.lookup_type("void").pointer().sizeof
        promise_offset = 2 * ptr_size
        promise_addr = int(promise_val.address)
        frame_base = promise_addr - promise_offset
        mem = gdb.selected_inferior().read_memory(frame_base, ptr_size)
        fmt = "<Q" if ptr_size == 8 else "<I"  # little-endian
        return struct.unpack(fmt, bytes(mem))[0]
    except Exception:
        return None


def _fn_name_at(addr):
    """Return the demangled outer-function name for a code address, or None.

    Strips GCC coroutine clone suffixes such as ' [clone .resume]' and
    ' [clone .destroy]' from the demangled name.
    """
    try:
        block = gdb.block_for_pc(addr)
        while block is not None:
            if block.function is not None:
                name = block.function.print_name
                # Remove GCC coroutine actor suffixes, e.g. " [clone .resume]"
                name = re.sub(r"\s*\[clone\s+\.\w+\]", "", name)
                return name
            block = block.superblock
    except Exception:
        pass
    return None


def _source_location_at(addr):
    """Return 'filename:line' string for a code address, or '??' on failure."""
    try:
        sal = gdb.find_pc_line(addr)
        if sal.symtab:
            return "{}:{}".format(sal.symtab.filename, sal.line)
    except Exception:
        pass
    return "??"


class PromiseTypePrinter(gdb.ValuePrinter):
    """GDB pretty printer for ecor::_promise_type<Task>."""

    def __init__(self, val):
        self.__val = val

    def to_string(self):
        addr = _resume_fn_addr(self.__val)
        if addr is None or addr == 0:
            return "<ecor promise: could not read resume fn>"

        name = _fn_name_at(addr) or "??"
        loc = _source_location_at(addr)
        result = "ecor::_promise_type [{}] @ {}".format(name, loc)

        # Follow _debug_parent chain (only present when compiled with ECOR_DEBUG_PARENT)
        seen = {int(self.__val.address)}
        val = self.__val
        try:
            while True:
                parent_ptr = val["_debug_parent"]
                parent_addr = int(parent_ptr)
                if parent_addr == 0 or parent_addr in seen:
                    break
                seen.add(parent_addr)
                parent_val = parent_ptr.dereference()
                p_fn_addr = _resume_fn_addr(parent_val)
                if p_fn_addr is None or p_fn_addr == 0:
                    break
                p_name = _fn_name_at(p_fn_addr) or "??"
                p_loc = _source_location_at(p_fn_addr)
                result += "\n    <- awaited by [{}] @ {}".format(p_name, p_loc)
                val = parent_val
        except gdb.error:
            pass  # _debug_parent field not present — compiled without ECOR_DEBUG_PARENT

        return result


# ---------------------------------------------------------------------------
# Registration
# ---------------------------------------------------------------------------


def _ecor_lookup(val):
    t = str(val.type.strip_typedefs())
    if re.search(r"ecor::_promise_type<", t):
        return PromiseTypePrinter(val)
    return None


gdb.pretty_printers.append(_ecor_lookup)
