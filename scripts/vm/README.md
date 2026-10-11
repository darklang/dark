# Work VM .NET build recovery

These are explicit build/test tools for VMs missing procfs and `/tmp`. They are
not runtime dependencies and must never be packaged with the interpreter.
The pinned SDK version/archive hash live in `Dockerfile`; its x64 hash is SHA512
despite the install script's `--amd64-sha256` option name. Extract as the current
user, with `tar --no-same-owner`.

`dotnet-metadata.c` supplies only current-process metadata needed by the SDK and
adapts `mkdtemp` to workspace temporary files. Metadata is synthetic: do not use
this launcher for benchmark/profiling measurements. Unrelated paths and errors
are delegated, not made accessible. No ptrace or system directory writes are used.

Compile with `cc -shared -fPIC -O2 scripts/vm/dotnet-metadata.c -ldl -o OUTPUT.so`.
Use the compiler repository's `scripts/vm/compat.c` adapter for stack discovery
and temporary-path mapping; its bootstrap emits `vm-compat.so`. Set
`DARK_VM_DOTNET_ROOT`, `DARK_VM_METADATA_SO`, and `DARK_VM_COMPAT_SO`, create the
SDK's `tmp` and `cli-home` directories, then invoke `bash scripts/vm/run-dotnet`.

Paket 10.3.1 can run from its `tools/net9.0/any/paket.dll` under the SDK's .NET 10
runtime. Restore packages before building. Build with `PaketExePath` pointing to
that DLL and `PaketDisableGlobalRestore=true`; **do not** set
`PaketRestoreDisabled=true`, which suppresses collecting the project references.
Use `-m:1 -nodeReuse:false` for SDK restore/build: discovering reusable MSBuild
nodes requires procfs. If a reset left `paket-files/paket.processlock`, first
ensure no Paket restore is active, then move that exact stale lock aside; do not
fake other-process information or delete the package cache.
After a build: run LocalExec migrations on a fresh scratch store, reload packages,
rebuild generated package hashes, then run tests from `backend/` with the normal
development environment and workspace-specific root/run-directory overrides.

GitHub branches, not SDK installs or scratch logs, are the recovery source after
a VM reset. Publish implementation checkpoints before lengthy setup/verification.
