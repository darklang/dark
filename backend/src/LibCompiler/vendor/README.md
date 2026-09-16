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

    cd <a scratch dir>
    git -C ~/code/compiler archive <commit from VENDORED-FROM> src/DarkCompiler | tar -x
    rm src/DarkCompiler/Program.fs src/DarkCompiler/DarkCompiler.fsproj
    mkdir -p a/backend/src b/backend/src
    mv src/DarkCompiler a/backend/src/LibCompiler
    cp -r <repo>/backend/src/LibCompiler b/backend/src/LibCompiler
    rm -rf b/backend/src/LibCompiler/{vendor,LibCompiler.fsproj,obj,bin}
    git diff --no-index --src-prefix=a/ --dst-prefix=b/ a b \
      | sed -E 's|^(---\|\+\+\+) ([ab])/[ab]/|\1 \2/|; s|^diff --git a/a/(\S+) b/b/|diff --git a/\1 b/|' \
      > <repo>/backend/src/LibCompiler/vendor/dark-fixes.patch

Then check it: apply it to a pristine copy and `diff -r` against the tree. The script
does the apply half of that on every re-vendor, so a stale patch fails loudly.

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
