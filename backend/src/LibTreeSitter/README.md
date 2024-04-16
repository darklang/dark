# LibTreeSitter

This project provides F# bindings for [tree-sitter](https://tree-sitter.github.io/tree-sitter/)
and [tree-sitter-darklang](/tree-sitter-darklang/).

The source was originally based on [`dotnet-tree-sitter`](https://github.com/profMagija/dotnet-tree-sitter).

## Quick explanation of what's here:

- a `.gitignore`'d `lib` directory containing `tree-sitter.so`, `tree-sitter-darklang.so`,
  and (if you're cross-compiling) platform-specific variants of these
  (e.g. `tree-sitter-linux-x64.so`)

  - `tree-sitter.so` is the built runtime library of tree-sitter.
    The `.so` file is built when the container is being built,
    and copied to this directory, either in CI or via VS Code devcontainer setup.
    This is pretty stable, and only needs to be rebuilt whenever we upgrade the
    tree-sitter version in the Dockerfile.

  - `tree-sitter-darklang.so` is the built Darklang parser,
    corresponding to [our grammar](/tree-sitter-darklang/grammar.js)
    and the resultant parser code. This is built whenever our grammar is updated.

- `TreeSitter.fs` is where all of the I/O happens against the `tree-sitter.so`,
as well as the F#-friendlier wrappers.

- `TreeSitter.Darklang.fs` is where all of the I/O happens against the `tree-sitter-darklang.so`, as well as the F#-friendlier wrappers.
