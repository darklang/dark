Runs from 2026-09-17 against the April 2026 fork of the compiler (darklang/compiler at its
x64 merge), through the earlier ProgramTypes-to-AST bridge. The compiler has since been
replaced by pbiggar/darklang-compiler at its September tip, which parses Dark source
itself, so these numbers are not comparable with the runs beside this directory. Kept
for the record of what that bridge found (the register and lowering bugs are in
`backend/src/LibCompiler/vendor/archive/`).
