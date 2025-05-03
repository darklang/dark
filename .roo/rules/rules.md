You're working on Darklang.
It's kind of like F# but with these key differences:
- try/catch doesn't exist
- rec keyword not needed
- DU values need the DU name included (not just None, but Option.None)

This whole respository/project is the source/only implementation of a software platform Darklang.

Darklang is a holistic solution for building software, with everything baked into one product.

Here are the specific parts, and where in the codebase to find them, if you end up needing to
- language definition (/backend/src/LibExecution/ProgramTypes.fs)
- runtime types (/backend/src/LibExecution/RuntimeTypes.fs)
- interpreter (/backend/src/LibExecution/Interpreter.fs)
- AST-lowering (/backend/src/LibExecution/ProgramTypesToRuntimeTypes.fs)
- builtins (infra. in RuntimeTypes.fs noted above, with an example in )
- package manager (infra. in RuntimeTypes.fs, impl. in /backend/src/LibPackageManager/PackageManager.fs)
- packages (currently in .dark files, and parsed whenever something in the packages/ dir changes). includes stdlib like math stuff, as well as pretty much anything else implemented in darklang
- language server (implemented _in Darklang_ in packages -- infra in packages/darklang/languageServerProtocol, impl in packages/darklang/languageTools/lsp-server)
- tree-sitter grammar (/tree-sitter-darklang)
- vs code extension (/vscode-extension)
- a CLI runtime (F# part in backend/src/Cli, Darklang part in packages/darklang/cli, with supporting stdlib in packages/darklang/stdlib/cli)

Any prompt you're given will be relevant usually to the development of Darklang, rather than the usage of darklang as an end-user developer.

Use that context, and pull in the details of specific files as relevant - don't eagerly fetch them now.