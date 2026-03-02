
.PHONY: build configure test clang-tidy size size-m0 size-report size-m0-report size-disasm size-m0-disasm

build:
	cmake --build --preset "debug"

configure:
	cmake --preset "debug" $(if $(SANITIZER), -DCMAKE_CXX_FLAGS="-fsanitize=$(SANITIZER)")

test: build
	ctest --preset "debug" --output-on-failure --verbose

clang-tidy:
	find include/ \( -iname "*.hpp" -or -iname "*.cpp" \) -print0 | parallel -0 clang-tidy -p build {}
# ── code-size analysis (requires arm-none-eabi-g++ in PATH) ─────────────────

SIZE_ELF      := build-size/ecor_size_probe
SIZE_ELF_M0   := build-size-m0/ecor_size_probe

size:
	cmake --workflow --preset size

size-m0:
	cmake --workflow --preset size-m0

size-report: $(SIZE_ELF)
	@./scripts/size_report.sh $(SIZE_ELF)

size-m0-report: $(SIZE_ELF_M0)
	@./scripts/size_report.sh $(SIZE_ELF_M0)
# Disassemble specific symbols.  Provide one or more patterns via DISASM=:
#   make size-disasm DISASM="task_d sink_err"
# Each space-separated token becomes a separate --disasm argument.
_DISASM_FLAGS := $(foreach p,$(DISASM),--disasm $(p))

size-disasm: $(SIZE_ELF)
	@./scripts/size_report.sh $(SIZE_ELF) $(_DISASM_FLAGS)

size-m0-disasm: $(SIZE_ELF_M0)
	@./scripts/size_report.sh $(SIZE_ELF_M0) $(_DISASM_FLAGS)
SIZE_ELF_OPT  := build-size-opt/ecor_size_probe

size-opt:
	cmake --workflow --preset size-opt

size-opt-report: $(SIZE_ELF_OPT)
	@./scripts/size_report.sh $(SIZE_ELF_OPT)

size-opt-disasm: $(SIZE_ELF_OPT)
	@./scripts/size_report.sh $(SIZE_ELF_OPT) $(_DISASM_FLAGS)

# Compare -Og baseline against -Og + size flags using arm-none-eabi-size.
size-compare: $(SIZE_ELF) $(SIZE_ELF_OPT)
	@echo "=== baseline (-Og) ==="
	@arm-none-eabi-size --format=berkeley $(SIZE_ELF)
	@echo "=== optimised (-Og + size flags) ==="
	@arm-none-eabi-size --format=berkeley $(SIZE_ELF_OPT)
