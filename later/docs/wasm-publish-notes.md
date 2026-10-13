<!--
RESTORE: AGENTS.md
WAS: the section on the wasm publish and how it fails
ANCHOR: a section of AGENTS.md, removed with backend/src/Wasm
ALSO: later/backend/src/Wasm/
WHEN: the first round of the molt, 2026-10-12
-->

# The wasm publish, and how it fails

Moved out of AGENTS.md by the molt, along with `backend/src/Wasm`. Kept because the failure
mode took a long time to diagnose and will take just as long the second time.

**A wasm publish piped to `tail` looks like a 40-minute hang.** `dotnet publish` spawns MSBuild
worker nodes with `/nodeReuse:true`. They inherit stdout and outlive the parent, so the pipe never
closes, `tail` never gets EOF, and whatever error dotnet printed sits in its buffer unseen: the
child exits, the shell stays, and `rundir/wasm-repl` stays empty with no exit code and no
diagnostic. Run it to a FILE with node reuse off, and nothing else building in that container,
since a Release wasm publish rebuilds the same project references and two of them fight over
obj/bin:

    MSBUILDDISABLENODEREUSE=1 dotnet publish backend/src/Wasm/Wasm.fsproj -c Release \
      -o rundir/wasm-repl -nodeReuse:false > rundir/logs/wasm-publish.log 2>&1

A real publish is 4 to 8 minutes and leaves hundreds of MB in `backend/Build/obj/Wasm` within the
first couple of minutes; `obj` still tiny means it never got past restore, which is not slowness.
Progress markers in the log, in order: restore, `Wasm -> ... Darklang.Wasm.dll`, `AOT'ing N
assemblies`, then the emscripten link and `wasm-opt`, which is single-threaded and the long tail.
(Diagnosed by the wasm-preview session, which named it from a one-line symptom.)

