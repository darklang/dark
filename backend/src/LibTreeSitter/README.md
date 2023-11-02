Bindings for [tree-sitter](https://tree-sitter.github.io/tree-sitter/).

(based on [`dotnet-tree-sitter`](https://github.com/profMagija/dotnet-tree-sitter))

---

Quick explanation of what's here:

- `Native.cs` is where all of the P/Invoke happens (the interop with the `tree-sitter.so` binary) - most of this is marked as `internal`
- the other files are wrappers around these to look nice within C#.

TODO: port to F# if this all pans out well
