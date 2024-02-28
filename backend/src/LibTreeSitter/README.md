# LibTreeSitter

This project provides F# bindings for [tree-sitter](https://tree-sitter.github.io/tree-sitter/)
and [tree-sitter-darklang](/tree-sitter-darklang/).

The source was originally based on [`dotnet-tree-sitter`](https://github.com/profMagija/dotnet-tree-sitter).

## Quick explanation of what's here:

A `.gitignore`'d `tree-sitter.so` should be present -- this is the built runtime
library of tree-sitter. The `.so` file is built when the container is being built,
and copied to this directory, either in CI or via VS Code devcontainer setup. This
is pretty stable, and only needs to be rebuilt whenever we upgrade the tree-sitter
version in the Dockerfile.

A `.gitignore`'d `tree-sitter-darklang.so` should be present -- this is the built
Darklang parser, corresponding to [our grammar](/tree-sitter-darklang/grammar.js)
and the resultant parser code. This is built whenever our grammar is updated.

`TreeSitter.Native.fs` is where all of the I/O happens against the `tree-sitter.so`,
all through P/Invoke. This aims to be super basic, a 1:1 mapping of the C bindings
exposed. Most of this is marked as `internal` so we don't try to access it elsewhere.

`LibTreeSitter.fs` provides a F#-friendly wrapper around `TreeSitter.Native`. We
should keep this pretty basic.

`LibTreeSitter.Darklang.fs` provies an F#-friendly wrapper around `TreeSitter.Native`
and `tree-sitter-darklang`. It's tiny, basically just exposing one function via P/Invoke.
