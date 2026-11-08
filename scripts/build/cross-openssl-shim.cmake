# The cmake toolchain for .NET's OpenSSL shim in a cross-linked static build. NativeAOT compiles
# that shim from source at publish time (StaticOpenSslLinking) by running cmake, which on its own
# builds it for the host. build-release-cli-exes.sh points CMAKE_TOOLCHAIN_FILE here and sets the
# two variables it reads.
set(CMAKE_SYSTEM_NAME Linux)
string(REGEX MATCH "^[^-]+" CMAKE_SYSTEM_PROCESSOR "$ENV{DARK_CROSS_TRIPLE}")
set(CMAKE_SYSROOT "$ENV{DARK_CROSS_SYSROOT}")
set(CMAKE_C_COMPILER clang)
set(CMAKE_C_COMPILER_TARGET "$ENV{DARK_CROSS_TRIPLE}")
# The host's ld can't link for another architecture, and cmake test-links while it configures.
set(CMAKE_EXE_LINKER_FLAGS_INIT -fuse-ld=lld)
# Headers and libraries from the sysroot only, never the host's OpenSSL.
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)
