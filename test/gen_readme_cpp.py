from enum import Enum
import sys


def load_file(file):
    with open(file, "r") as fd:
        while line := fd.readline():
            yield line


class snippet(Enum):
    START = 1
    END = 2


def find_snippets(lines):
    gen_stuff = False
    for line in lines:
        if "```cpp" in line:
            yield snippet.START
            gen_stuff = True
            continue
        elif "```" in line:
            yield snippet.END
            gen_stuff = False
            continue
        elif gen_stuff:
            if "error:" in line:
                yield "// " + line
            else:
                yield line
        else:
            yield "// " + line


def gen_cpp(lines):
    yield f"""
    #include <ecor/ecor.hpp>

    #include "doctest.h"
    #include <vector>
    #include <cstdint>
    #include <string>
    #include <variant>
    #include <optional>
    #include <functional>

    namespace ecor{{

    // Mock functions for examples
    void turn_on_led() {{}}
    void turn_off_led() {{}}
    void toggle_led() {{}}
    int read_button_id() {{ return 1; }}
    uint64_t get_current_time() {{ return 0; }}
    uint64_t get_time_ms() {{ return 0; }}
    int read_temperature() {{ return 25; }}
    void process_temperature(int) {{}}
    int read_temperature_sensor() {{ return 25; }}
    void process_message(auto&&) {{}}
    void __WFI() {{}}
    void cleanup_resources() {{}}
    void close_file() {{}}
    void disable_interrupt() {{}}
    void flush_buffers() {{}}
    void* custom_alloc(size_t, size_t) {{ return nullptr; }}
    void custom_free(void*, size_t, size_t) {{}}

    ecor::task<void> my_task(ecor::task_ctx&) {{ co_return; }}
    ecor::task<void> another_task(ecor::task_ctx&) {{ co_return; }}
    ecor::task<int> might_fail(ecor::task_ctx&) {{ co_return 42; }}

    """

    for i, l in enumerate(lines):
        if l == snippet.START:
            yield f"""
                namespace t_{i} {{
            """
        elif l == snippet.END:
            yield f"""
                }}
            """
        else:
            yield l

    yield f"""
    }}

    """


inpt_md = sys.argv[1]
outpt_cpp = sys.argv[2]

with open(outpt_cpp, "w") as fd:
    for out in gen_cpp(find_snippets(load_file(inpt_md))):
        fd.write(out)
