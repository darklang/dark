# What this directory is

`backend/src/LibCompiler/` is a copy of the native compiler,
[pbiggar/darklang-compiler](https://github.com/pbiggar/darklang-compiler)'s
`src/DarkCompiler/`, put there by `scripts/build/vendor-compiler`. It is not tracked
by git (see `.gitignore`); only this `vendor/` directory is, and `VENDORED-FROM` says
which commit the copy came from, and whether it was a working tree with edits.

The rule that keeps this sane: **compiler code lives in the compiler checkout**. A
fix to the compiler is made in `~/code/compiler` on a branch there, copied here with
`vendor-compiler --worktree` while it is being worked on, and sent upstream as a PR.
Nothing is patched on the way in any more; the patch we used to carry against the
April fork is under `archive/` for the record and does not apply to the current tree.

    scripts/build/vendor-compiler ~/code/compiler              # its HEAD commit
    scripts/build/vendor-compiler ~/code/compiler paul/main    # a specific commit
    scripts/build/vendor-compiler --worktree ~/code/compiler   # the working tree, edits included

Then build flag-on (both, so package code that references the compiler builtins
resolves at reload):

    ./scripts/build/_dotnet-wrapper build --configuration Debug \
        -p:DarkWithCompiler=true src/Cli/Cli.fsproj src/LocalExec/LocalExec.fsproj

Before sending a compiler change upstream, run its own suite here:
`scripts/compiler/upstream-tests`.
