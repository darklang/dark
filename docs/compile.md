# Compiling to an executable

`dark compile` turns a package fn into a native executable with the Darklang
compiler (darklang/compiler). It is experimental.

```bash
$ dark compile Stdlib.Cli.Path.join
wrote ./join (327 KB) in 2.4 s
$ ./join usr local bin
usr/local/bin
```

## Which fns can be executables

A fn that takes `List<String>` gets the command-line arguments. A fn that takes
nothing is called with nothing. The executable prints what `dark eval` prints for
that call. Any other signature is refused:

```bash
$ dark compile Stdlib.String.padStart
Stdlib.String.padStart(string: String, padWith: Char, goalLength: Int64): String can't be an executable yet: it needs to take List<String>, or nothing.
```

## What compiles

The fn and every package fn, type and value it uses are read from this instance's
main branch. Most of the stdlib compiles. Fns that need something only the
interpreter has, like the store, source control, traces or the parser, do not.
`dark compile --check <fn>...` says whether each fn compiles, and if not, why.

`dark compile --run <expression>` compiles an expression, runs it, and prints only
what it prints.

## The command

```text
dark compile <fn>                write ./<last part of the fn's name>
dark compile <fn> -o <file>      write <file>
dark compile --run <expression>  compile, run, print what it prints
dark compile --check <fn>...     does each fn compile
  -q                             nothing on stderr
  -v, -vv, -vvv                  also the compiler's passes, their times, every intermediate form
```

## What an executable is

An executable is a standalone tool. It has no store, no traces and no DB, and it
does not check this instance's permission policy: it runs with the access of
whoever runs it.

## Platforms

The compiler and the executables it writes run on Linux, x86-64 or arm64, with
glibc 2.38 or newer.

## The compiler

`dark compile` uses the compiler at `~/.darklang/compiler/<commit>/darkc`, where
`<commit>` is the pinned darklang/compiler commit (`pinnedCommit` in
`packages/darklang/cli/compile.dark`). `scripts/compiler/build` builds it there.
`DARK_COMPILER` points at another one.

The first compile with a compiler prepares its stdlib, which takes about a minute
and 150 MB in `~/.cache/dark-compiler`. It asks first. Later compiles take a few
seconds.

While a compile runs, the instance serves its packages to the compiler on a random
loopback port, with a one-time token so other local processes can't read the store
through it.
