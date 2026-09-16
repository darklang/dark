# What this directory is

`backend/src/LibCompiler/` is [darklang/compiler](https://github.com/darklang/compiler)'s
`src/DarkCompiler/`, copied in. It is not a fork with history; `VENDORED-FROM` names the
upstream commit, and `*.patch` is everything we have changed on top of it.

Two rules keep that true:

- Don't edit LibCompiler in place and leave it. Either send the change upstream and
  re-vendor, or fold it into a patch here. `scripts/build/vendor-compiler` refuses to run
  over uncommitted edits so a fix can't be lost by accident.
- Re-vendor with the script, not by hand:

      scripts/build/vendor-compiler ~/code/compiler            # its HEAD
      scripts/build/vendor-compiler ~/code/compiler origin/main

## Regenerating `dark-fixes.patch`

After changing something in LibCompiler that isn't going upstream yet:

    scripts/build/vendor-compiler --regen-patch ~/code/compiler

It diffs the commit in `VENDORED-FROM` against the tree as it is now. The script's
normal mode applies the patch on every re-vendor, so a stale one fails loudly.

## What is in `dark-fixes.patch` today

Found while bridging real package fns (PR 5690), not upstream yet:

- `passes/x64/6_CodeGen.fs`: `HeapStore` of a string literal clobbered RCX (an allocatable
  register); unsigned compare/div/mod lowered signed; `FArgMoves` skipped parallel-move
  resolution, so a cyclic float shuffle like `f(x, g())` computed `f(x, x)`; `File.delete`
  was a stub.
- `passes/1.5_TypeChecking.fs`: tvar unification instead of structural equality in
  Apply/FuncRef.
- `passes/2_AST_to_ANF.fs`: variant-tag lookup scoped by type; lambda-lift inference
  leniency.
- `passes/4_MIR_to_LIR.fs`, `5_RegisterAllocation.fs`, `arm64/6_CodeGen.fs`, `LIR.fs`:
  XMM15 reserved as float scratch for the FArgMoves fix.
- `stdlib/`: `Rpc.dark` (the hostRpc seam primitives), `Float.fromBits`, `String.toBytes`.
- `CompilerLibrary.fs`: entry points the bridge calls.
