# ARM bare-metal cross-compilation toolchain for ecor code-size analysis.
#
# Configurable cache variables (set via -D or in a preset): ARM_CPU        —
# target CPU, default "cortex-m4" ARM_FPU        — FPU variant, default
# "fpv4-sp-d16" (set to "none" for soft-float) ARM_FLOAT_ABI  — "hard" |
# "softfp" | "soft", default "hard"

cmake_minimum_required(VERSION 3.19)

set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_SYSTEM_PROCESSOR arm)

# ── toolchain binaries ───────────────────────────────────────────────────────
set(CMAKE_C_COMPILER arm-none-eabi-gcc)
set(CMAKE_CXX_COMPILER arm-none-eabi-g++)
set(CMAKE_ASM_COMPILER arm-none-eabi-gcc)
set(CMAKE_AR arm-none-eabi-ar)
set(CMAKE_RANLIB arm-none-eabi-ranlib)
set(CMAKE_OBJDUMP
    arm-none-eabi-objdump
    CACHE STRING "")
set(CMAKE_SIZE_UTIL
    arm-none-eabi-size
    CACHE STRING "")
set(CMAKE_NM
    arm-none-eabi-nm
    CACHE STRING "")

# Keep CMake from trying to link test executables with the cross-compiler.
set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)

# ── CPU / FPU flags ─────────────────────────────────────────────────────────
if(NOT DEFINED ARM_CPU)
  set(ARM_CPU
      "cortex-m4"
      CACHE STRING "Target Cortex-M CPU (e.g. cortex-m0, cortex-m4, cortex-m7)")
endif()
if(NOT DEFINED ARM_FPU)
  set(ARM_FPU
      "fpv4-sp-d16"
      CACHE STRING "FPU variant, 'none' for soft-float only")
endif()
if(NOT DEFINED ARM_FLOAT_ABI)
  set(ARM_FLOAT_ABI
      "hard"
      CACHE STRING "Float ABI: hard, softfp, soft")
endif()

set(_CPU_FLAGS "-mcpu=${ARM_CPU} -mthumb")

if(NOT ARM_FPU STREQUAL "none")
  string(APPEND _CPU_FLAGS " -mfpu=${ARM_FPU} -mfloat-abi=${ARM_FLOAT_ABI}")
else()
  string(APPEND _CPU_FLAGS " -mfloat-abi=soft")
endif()

# ── global compile flags ─────────────────────────────────────────────────────
# -ffunction-sections / -fdata-sections: one section per symbol so the linker
# can strip exactly what is not reachable from the entry point.
set(CMAKE_C_FLAGS_INIT "${_CPU_FLAGS} -ffunction-sections -fdata-sections")
set(CMAKE_CXX_FLAGS_INIT
    "${_CPU_FLAGS} -ffunction-sections -fdata-sections -fno-exceptions -fno-rtti"
)
set(CMAKE_ASM_FLAGS_INIT "${_CPU_FLAGS}")

# ── linker flags ─────────────────────────────────────────────────────────────
# --gc-sections: dead-code elimination at link time. -Map: produce a linker map
# file alongside the ELF for extra analysis. --specs=nosys.specs: stub out OS
# syscalls (no semihosting). --specs=nano.specs: use newlib-nano for minimal C
# library footprint.
set(CMAKE_EXE_LINKER_FLAGS_INIT
    "${_CPU_FLAGS} -Wl,--gc-sections -Wl,-Map,\${TARGET_NAME}.map \
--specs=nosys.specs --specs=nano.specs")
