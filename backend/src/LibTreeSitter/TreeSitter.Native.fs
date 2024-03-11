/// Raw F# bindings (P/Invoke) for tree-sitter
module LibTreeSitter_FS.TreeSitter

// we need this to make F# OK with the Explicit LayoutKinds
#nowarn "9"

open System
open System.Runtime.InteropServices

// TODO: some of these types and functions could be internal
// please see source of dotnet-treesitter-bindings for details
// (Native.cs in particular)

module Types =
  type TsInputEncoding =
    | Utf8 = 0
    | Utf16 = 1

  [<Struct; StructLayout(LayoutKind.Sequential)>]
  type TsPoint =
    val mutable row : uint32
    val mutable column : uint32

  // 32 = 4 * 4 + 8 + 8
  [<StructLayout(LayoutKind.Explicit, Size = 32)>]
  type TsNode =
    // 16 = 4 * 4
    [<FieldOffset(16)>]
    val mutable id : IntPtr

    // 24 = 4 * 4 + 8
    [<FieldOffset(24)>]
    val mutable tree : IntPtr

  // 24 = 8 + 8 + 2 * 4
  [<StructLayout(LayoutKind.Explicit, Size = 24)>]
  type TsTreeCursor =
    [<FieldOffset(0)>]
    val mutable tree : IntPtr
    [<FieldOffset(8)>]
    val mutable id : IntPtr


[<Literal>]
let private DllName = "tree-sitter"

module Parser =
  [<DllImport(DllName)>]
  extern IntPtr ts_parser_new()

  [<DllImport(DllName)>]
  extern void ts_parser_delete(IntPtr parser)

  [<DllImport(DllName)>]
  extern bool ts_parser_set_language(IntPtr self, IntPtr language)

  [<DllImport(DllName)>]
  extern IntPtr ts_parser_language(IntPtr self)

  [<DllImport(DllName)>]
  extern IntPtr ts_parser_parse_string_encoding(
    IntPtr self,
    IntPtr oldTree,
    IntPtr input,
    uint32 length,
    TsInputEncoding encoding
  )

  [<DllImport(DllName)>]
  extern void ts_parser_set_timeout_micros(IntPtr self, uint64 timeout)

  [<DllImport(DllName)>]
  extern uint64 ts_parser_timeout_micros(IntPtr self)

  [<DllImport(DllName)>]
  extern void ts_parser_set_cancellation_flag(IntPtr self, IntPtr flag)

module Tree =
  [<DllImport(DllName)>]
  extern void ts_tree_delete(IntPtr self)

  [<DllImport(DllName)>]
  extern TsNode ts_tree_root_node(IntPtr self)

module Node =
  [<DllImport(DllName)>]
  extern IntPtr ts_node_type(TsNode node)

  [<DllImport(DllName)>]
  extern TsPoint ts_node_start_point(TsNode node)

  [<DllImport(DllName)>]
  extern TsPoint ts_node_end_point(TsNode node)

  [<DllImport(DllName)>]
  extern IntPtr ts_node_string(TsNode node)

  [<DllImport(DllName)>]
  extern uint ts_node_child_count(TsNode node)

  [<DllImport(DllName)>]
  extern bool ts_node_eq(TsNode node, TsNode other)


module TreeCursor =
  [<DllImport(DllName)>]
  extern TsTreeCursor ts_tree_cursor_new(TsNode node)

  [<DllImport(DllName)>]
  extern void ts_tree_cursor_delete(TsTreeCursor& cursor)

  [<DllImport(DllName)>]
  extern TsNode ts_tree_cursor_current_node(TsTreeCursor& self)

  [<DllImport(DllName)>]
  extern IntPtr ts_tree_cursor_current_field_name(TsTreeCursor& self)

  [<DllImport(DllName)>]
  extern bool ts_tree_cursor_goto_parent(TsTreeCursor& self)

  [<DllImport(DllName)>]
  extern bool ts_tree_cursor_goto_next_sibling(TsTreeCursor& self)

  [<DllImport(DllName)>]
  extern bool ts_tree_cursor_goto_first_child(TsTreeCursor& self)


[<DllImport(DllName)>]
extern void ts_util_free(IntPtr mem)
