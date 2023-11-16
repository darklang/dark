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

  type TsSymbolType =
    | Regular = 0
    | Anonymous = 1
    | Auxiliary = 2

  [<Struct; StructLayout(LayoutKind.Sequential)>]
  type TsPoint =
    val mutable row : uint32
    val mutable column : uint32

  [<Struct; StructLayout(LayoutKind.Sequential)>]
  type TsRange =
    val mutable start_point : TsPoint
    val mutable end_point : TsPoint
    val mutable start_byte : uint32
    val mutable end_byte : uint32

  // internal delegate IntPtr TsReadDelegate(IntPtr payload, uint byteIndex, TsPoint position, out uint bytesRead);
  type TsReadDelegate = delegate of IntPtr * uint32 * TsPoint -> IntPtr * uint32

  [<Struct; StructLayout(LayoutKind.Sequential)>]
  type TsInput =
    val mutable payload : IntPtr
    val mutable read : TsReadDelegate
    val mutable encoding : TsInputEncoding

  type TsLogType =
    | Parse = 0
    | Lex = 1

  // internal delegate void TsLogDelegate(IntPtr payload, TsLogType logType, IntPtr data);
  type TsLogDelegate = delegate of IntPtr * TsLogType * IntPtr -> unit

  [<Struct; StructLayout(LayoutKind.Sequential)>]
  type TsLogger =
    val mutable payload : IntPtr
    val mutable log : TsLogDelegate

  [<Struct; StructLayout(LayoutKind.Sequential)>]
  type TsInputEdit =
    val mutable start_byte : uint32
    val mutable old_end_byte : uint32
    val mutable new_end_byte : uint32
    val mutable start_point : TsPoint
    val mutable old_end_point : TsPoint
    val mutable new_end_point : TsPoint

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

  [<StructLayout(LayoutKind.Sequential)>]
  type TsQueryCapture =
    val mutable node : TsNode
    val mutable index : uint32

  [<StructLayout(LayoutKind.Sequential)>]
  type TsQueryMatch =
    val mutable id : uint32
    val mutable pattern_index : uint16
    val mutable capture_count : uint16
    val mutable captures : IntPtr

  type TsQueryPredicateStepType =
    | Done
    | Capture
    | String

  [<StructLayout(LayoutKind.Sequential)>]
  type TsQueryPredicateStep =
    // Note: this was just `type` originally
    val mutable type_ : TsQueryPredicateStepType
    val mutable value_id : uint32

  type TsQueryError =
    | None = 0
    | Syntax = 1
    | NodeType = 2
    | Field = 3
    | Capture = 4



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

  // TODO
  // [<DllImport(DllName)>]
  // extern void ts_parser_set_included_ranges(
  //       IntPtr self,
  //       [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)>] ranges: TsRange[],
  //       length: uint32)

  // [<DllImport(DllName)>]
  // extern TsRange[] ts_parser_included_ranges(
  //     IntPtr self,
  //     [<Out>] length: byref<uint32>)

  // #endregion

  [<DllImport(DllName)>]
  extern IntPtr ts_parser_parse(IntPtr self, IntPtr oldTree, TsInput input)

  [<DllImport(DllName)>]
  extern IntPtr ts_parser_parse_string(
    IntPtr self,
    IntPtr oldTree,
    IntPtr input,
    uint32 length
  )

  [<DllImport(DllName)>]
  extern IntPtr ts_parser_parse_string_encoding(
    IntPtr self,
    IntPtr oldTree,
    IntPtr input,
    uint32 length,
    TsInputEncoding encoding
  )

  [<DllImport(DllName)>]
  extern void ts_parser_reset(IntPtr self)

  [<DllImport(DllName)>]
  extern void ts_parser_set_timeout_micros(IntPtr self, uint64 timeout)

  [<DllImport(DllName)>]
  extern uint64 ts_parser_timeout_micros(IntPtr self)

  [<DllImport(DllName)>]
  extern void ts_parser_set_cancellation_flag(IntPtr self, IntPtr flag)

  [<DllImport(DllName)>]
  extern void ts_parser_set_logger(IntPtr self, TsLogger logger)

  [<DllImport(DllName)>]
  extern TsLogger ts_parser_get_logger(IntPtr self)

  [<DllImport(DllName)>]
  extern void ts_parser_print_dot_graphs(IntPtr self, int file)

module Tree =
  [<DllImport(DllName)>]
  extern IntPtr ts_tree_copy(IntPtr self)

  [<DllImport(DllName)>]
  extern void ts_tree_delete(IntPtr self)

  [<DllImport(DllName)>]
  extern TsNode ts_tree_root_node(IntPtr self)

  [<DllImport(DllName)>]
  extern IntPtr ts_tree_language(IntPtr self)

  [<DllImport(DllName)>]
  extern IntPtr ts_tree_edit(IntPtr self, TsInput& edit)

module Node =
  [<DllImport(DllName)>]
  extern IntPtr ts_node_type(TsNode node)

  [<DllImport(DllName)>]
  extern uint16 ts_node_symbol(TsNode node)

  [<DllImport(DllName)>]
  extern uint ts_node_start_byte(TsNode node)

  [<DllImport(DllName)>]
  extern TsPoint ts_node_start_point(TsNode node)

  [<DllImport(DllName)>]
  extern uint ts_node_end_byte(TsNode node)

  [<DllImport(DllName)>]
  extern TsPoint ts_node_end_point(TsNode node)

  [<DllImport(DllName)>]
  extern IntPtr ts_node_string(TsNode node)

  [<DllImport(DllName)>]
  extern bool ts_node_is_null(TsNode node)

  [<DllImport(DllName)>]
  extern bool ts_node_is_named(TsNode node)

  [<DllImport(DllName)>]
  extern bool ts_node_is_missing(TsNode node)

  [<DllImport(DllName)>]
  extern bool ts_node_is_extra(TsNode node)

  [<DllImport(DllName)>]
  extern bool ts_node_has_changes(TsNode node)

  [<DllImport(DllName)>]
  extern bool ts_node_has_error(TsNode node)

  // [<DllImport(DllName), MarshalAs(UnmanagedType.Struct)>]
  // extern TsNode ts_node_parent(TsNode node)

  // [<DllImport(DllName), MarshalAs(UnmanagedType.Struct)>]
  // extern TsNode ts_node_child(TsNode node, uint index)

  [<DllImport(DllName)>]
  extern uint ts_node_child_count(TsNode node)

  // [<DllImport(DllName), MarshalAs(UnmanagedType.Struct)>]
  // extern TsNode ts_node_named_child(TsNode node, uint index)

  [<DllImport(DllName)>]
  extern uint ts_node_named_child_count(TsNode node)

  // [<DllImport(DllName), MarshalAs(UnmanagedType.Struct)>]
  // extern TsNode ts_node_child_by_field_name(TsNode node, IntPtr fieldName, uint fieldNameLength)

  // [<DllImport(DllName), MarshalAs(UnmanagedType.Struct)>]
  // extern TsNode ts_node_child_by_field_id(TsNode node, ushort fieldId)

  // [<DllImport(DllName), MarshalAs(UnmanagedType.Struct)>]
  // extern TsNode ts_node_next_sibling(TsNode node)

  // [<DllImport(DllName), MarshalAs(UnmanagedType.Struct)>]
  // extern TsNode ts_node_prev_sibling(TsNode node)

  // [<DllImport(DllName), MarshalAs(UnmanagedType.Struct)>]
  // extern TsNode ts_node_next_named_sibling(TsNode node)

  // [<DllImport(DllName), MarshalAs(UnmanagedType.Struct)>]
  // extern TsNode ts_node_prev_named_sibling(TsNode node)

  // [<DllImport(DllName), MarshalAs(UnmanagedType.Struct)>]
  // extern TsNode ts_node_first_named_child_for_byte(TsNode node, uint offset)

  // [<DllImport(DllName), MarshalAs(UnmanagedType.Struct)>]
  // extern TsNode ts_node_descendant_for_byte_range(TsNode node, uint start, uint end)

  // [<DllImport(DllName), MarshalAs(UnmanagedType.Struct)>]
  // extern TsNode ts_node_descendant_for_point_range(TsNode node, TsPoint start, TsPoint end)

  // [<DllImport(DllName), MarshalAs(UnmanagedType.Struct)>]
  // extern TsNode ts_node_named_descendant_for_byte_range(TsNode node, uint start, uint end)

  // [<DllImport(DllName), MarshalAs(UnmanagedType.Struct)>]
  // extern TsNode ts_node_named_descendant_for_point_range(TsNode node, TsPoint start, TsPoint end)

  [<DllImport(DllName)>]
  extern void ts_node_edit(TsNode& node, TsInputEdit& edit)

  [<DllImport(DllName)>]
  extern bool ts_node_eq(TsNode node, TsNode other)


module TreeCursor =
  [<DllImport(DllName)>]
  extern TsTreeCursor ts_tree_cursor_new(TsNode node)

  [<DllImport(DllName)>]
  extern void ts_tree_cursor_delete(TsTreeCursor& cursor)

  [<DllImport(DllName)>]
  extern TsTreeCursor ts_tree_cursor_reset(TsTreeCursor& self, TsNode node)

  [<DllImport(DllName)>]
  extern TsNode ts_tree_cursor_current_node(TsTreeCursor& self)

  [<DllImport(DllName)>]
  extern IntPtr ts_tree_cursor_current_field_name(TsTreeCursor& self)

  [<DllImport(DllName)>]
  extern uint16 ts_tree_cursor_current_field_id(TsTreeCursor& self)

  [<DllImport(DllName)>]
  extern bool ts_tree_cursor_goto_parent(TsTreeCursor& self)

  [<DllImport(DllName)>]
  extern bool ts_tree_cursor_goto_next_sibling(TsTreeCursor& self)

  [<DllImport(DllName)>]
  extern bool ts_tree_cursor_goto_first_child(TsTreeCursor& self)

  [<DllImport(DllName)>]
  extern uint64 ts_tree_cursor_goto_first_child_for_byte(
    TsTreeCursor& self,
    uint32 offset
  )

  [<DllImport(DllName)>]
  extern TsTreeCursor ts_tree_cursor_copy(TsTreeCursor& self)

module Language =

  [<DllImport(DllName)>]
  extern uint32 ts_language_symbol_count(IntPtr language)

  [<DllImport(DllName)>]
  extern IntPtr ts_language_symbol_name(IntPtr language, uint16 symbol)

  [<DllImport(DllName)>]
  extern uint16 ts_language_symbol_for_name(
    IntPtr language,
    IntPtr name,
    uint32 length,
    bool isNamed
  )

  [<DllImport(DllName)>]
  extern uint16 ts_language_field_count(IntPtr language)

  [<DllImport(DllName)>]
  extern IntPtr ts_language_field_name_for_id(IntPtr language, uint16 fieldId)

  [<DllImport(DllName)>]
  extern uint16 ts_language_field_id_for_name(
    IntPtr language,
    IntPtr name,
    uint32 length
  )

  [<DllImport(DllName)>]
  extern Types.TsSymbolType ts_language_symbol_type(IntPtr language, uint16 symbol)

  [<DllImport(DllName)>]
  extern uint32 ts_language_version(IntPtr language)

[<DllImport(DllName)>]
extern void ts_util_free(IntPtr mem)
