namespace LibTreeSitter.Darklang

open System
open System.Runtime.InteropServices

open LibTreeSitter.Main

module DarklangLanguage =
    [<Literal>]
    let private dllName = "tree-sitter-darklang"

    [<DllImport(dllName, CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr tree_sitter_darklang()

    let Create() = Language(tree_sitter_darklang())
