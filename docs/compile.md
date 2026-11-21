# Compiling to an executable

`dark compile` turns a package fn into a native executable with the Darklang
compiler (darklang/compiler). It is experimental.

```bash
$ dark compile Me.Tools.wc
wrote ./wc (341 KB) in 2.1 s
$ ./wc notes.txt
112 notes.txt
```

## Which fns can be executables

The same fns `dark run <fn>` runs. The fn takes `List<String>`, the command-line
arguments, or nothing, and returns `Int64`, the exit code, or `Unit`, which is 0.
`./wc a b` does what `dark run Me.Tools.wc a b` does. Any other fn is refused:

```bash
$ dark compile Stdlib.String.padStart
Stdlib.String.padStart takes (string: String, padWith: Char, goalLength: Int64); an executable's fn takes List<String> or nothing, as for `dark run`.
```

`dark compile --run <expression>` compiles an expression, runs it, and prints
what `dark eval` prints.

## What an executable can do

An executable has no store, no traces and no DB. A fn that reads the package
store, records traces or uses dark's own runtime state is refused before
anything compiles, with the calls that lead there:

```bash
$ dark compile Me.Tools.listFns
Me.Tools.listFns can't be an executable: it reads the package store, which only a Dark instance has.
  Me.Tools.listFns -> Darklang.LanguageTools.PackageManager.Search.search -> Builtin.pmSearch
`dark run Me.Tools.listFns` runs it here.
```

Every access is off by default, and compiled code can't limit access yet, so an
executable gets only stdin, stdout, the clock and randomness. A fn that reads
files, the environment or the network, or starts programs, is refused for now.
`dark effects <fn>` lists what a fn may do.

## What compiles

The fn and every package fn, type and value it uses are read from this instance's
main branch. Most of the stdlib compiles. `dark compile --check <fn>...` says, for
each fn, whether it compiles and if not, why; `--json` gives one object per fn.

```text
compiles      Stdlib.English.toOrdinal   no effects
needs dark    Me.Tools.listFns           reads the package store, via ...
not yet       Me.Tools.cat               reads files, via Darklang.Stdlib.Cli.File.readText -> ...
```

## The command

```text
dark compile <fn>                       write ./<last part of the fn's name>
dark compile <fn> -o <file>             write <file>
dark compile --run <expression>         compile, run, print what it prints
dark compile --check [--json] <fn>...   does each fn compile
  -q                                    nothing on stderr
  -v, -vv, -vvv                         also the compiler's passes, their times, every intermediate form
```

## Platforms

The compiler runs on Linux x86-64 and arm64, and macOS arm64. Elsewhere,
`dark compile` says so before doing anything.

## Where things live

The compiler is `~/.darklang/compiler/<commit>/darkc`, where `<commit>` is the
first 9 characters of the pinned darklang/compiler commit (`pinnedCommit` in
`packages/darklang/compiler/darkc.dark`). A release CLI for Linux carries it and
writes it there the first time it starts, and removes other pins' directories. In
a dev build, `scripts/compiler/build` builds it there.

A Release build embeds `rundir/compiler/<commit>/<runtime>/darkc.br` when it is
there: `scripts/compiler/embed <static darkc> linux-x64 linux-musl-x64` writes it.
`scripts/compiler/fetch` downloads the static builds from the compiler's release
`darkc-<commit>`, which has Linux x86-64 and arm64 builds. Nothing is embedded for
macOS or linux-arm.

The compiler's stdlib is prepared once: about a minute and 150 MB, in
`~/.darklang/compiler/<commit>/cache`. The first `dark` at a terminal starts it in
the background on Linux, when 6 GB of memory is free; otherwise the first compile
does it. `dark config set compile.prepare off` turns the background start off.
Later compiles take a few seconds.

## Internals

`Darklang.Compiler.Darkc` holds every flag `dark compile` passes to darkc:

```text
darkc --cache-dir D --package-server URL -q --entry <fn hash> -o <file>
darkc --cache-dir D --package-server URL -q -r -e -- <expression>
darkc --cache-dir D --package-server URL --check --json <name>...
```

The entry is the fn's hash. darkc passes it the command-line words and turns its
result into the exit code. Dark checks the signature first, with the rule
`dark run` uses (`Darklang.Cli.Run.entry`).

Before darkc starts, `Darklang.Compiler.Access` reads the fn's effects from the
analysis behind `dark effects` (`Builtin.pmFnEffectReasons`). `PackageRead`,
`PackageWrite`, `TraceRead`, `TraceWrite` and `Runtime` need an instance. Every
other effect except `Stdin`, `Stdout`, `Clock`, `Random` and `Raise` waits until
compiled code enforces access.

Access past stdin, stdout, the clock and randomness is approved the way `dark run`
approves it, for the fn's name and version, and saved in the same place: at a
terminal `dark compile` asks (allow this build, remember for this code version,
cancel); without one it prints the `dark permissions approve` line to run first.
The approved policy goes to `~/.darklang/compiler/<commit>/policies/<fn hash>.json`
and to darkc as `--policy FILE`. It is `LanguageTools.Permissions.Policy` as
`Stdlib.Json.serialize` writes it:

```json
{"allow":[{"Effect":[{"FileRead":[]}]}],"deny":[]}
```

darkc is to bake it into the executable and check it where compiled code reaches
the OS: opening, writing and removing files, connecting, binding, starting a
child, reading or setting an environment variable. A denial prints what
`dark run` prints for it and exits 1. Until the pinned darkc does that
(`Darkc.enforcesAccess`), such fns are refused instead.

While a compile runs, the instance serves its packages to the compiler on a random
loopback port, with a one-time token so other local processes can't read the store
through it.
