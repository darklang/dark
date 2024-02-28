using System;
using System.Runtime.InteropServices;

namespace LibTreeSitter.Native
{
  internal enum TsInputEncoding
  {
    Utf8,
    Utf16
  }

  internal enum TsSymbolType
  {
    Regular,
    Anonymous,
    Auxiliary
  }

  [StructLayout(LayoutKind.Sequential)]
  internal struct TsPoint
  {
    public uint row;
    public uint column;
  }

  [StructLayout(LayoutKind.Sequential)]
  internal struct TsRange
  {
    public TsPoint start_point;
    public TsPoint end_point;
    public uint start_byte;
    public uint end_byte;
  }

  internal delegate IntPtr TsReadDelegate(IntPtr payload, uint byteIndex, TsPoint position, out uint bytesRead);

  [StructLayout(LayoutKind.Sequential)]
  internal struct TsInput
  {
    public IntPtr payload;
    public TsReadDelegate read;
    public TsInputEncoding encoding;
  }

  internal enum TsLogType
  {
    Parse,
    Lex
  }

  internal delegate void TsLogDelegate(IntPtr payload, TsLogType logType, IntPtr data);

  [StructLayout(LayoutKind.Sequential)]
  internal struct TsLogger
  {
    public IntPtr payload;
    public TsLogDelegate log;
  }

  [StructLayout(LayoutKind.Sequential)]
  internal struct TsInputEdit
  {
    uint start_byte;
    uint old_end_byte;
    uint new_end_byte;
    TsPoint start_point;
    TsPoint old_end_point;
    TsPoint new_end_point;
  }

  [StructLayout(LayoutKind.Explicit, Size = 4 * 4 + 8 + 8)]
  internal struct TsNode
  {
    [FieldOffset(4 * 4)] public IntPtr id;
    [FieldOffset(4 * 4 + 8)] public IntPtr tree;
  }

  [StructLayout(LayoutKind.Explicit, Size = 8 + 8 + 2 * 4)]
  internal struct TsTreeCursor
  {
    [FieldOffset(0)] public IntPtr tree;
    [FieldOffset(8)] public IntPtr id;
  }

  internal static class Native
  {
    private const string DllName = "tree-sitter";

    #region Parser

    [DllImport(DllName)]
    internal static extern IntPtr ts_parser_new();

    [DllImport(DllName)]
    internal static extern void ts_parser_delete(IntPtr parser);

    [DllImport(DllName)]
    internal static extern bool ts_parser_set_language(IntPtr self, IntPtr language);

    [DllImport(DllName)]
    internal static extern IntPtr ts_parser_language(IntPtr self);

    [DllImport(DllName)]
    internal static extern void ts_parser_set_included_ranges(
        IntPtr self,
        [MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 2)]
            TsRange[] ranges,
        uint length);

    [DllImport(DllName)]
    [return: MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)]
    internal static extern TsRange[] ts_parser_included_ranges(
        IntPtr self,
        out uint length);

    [DllImport(DllName)]
    internal static extern IntPtr ts_parser_parse(
        IntPtr self,
        IntPtr oldTree,
        TsInput input
    );

    [DllImport(DllName)]
    internal static extern IntPtr ts_parser_parse_string(
        IntPtr self,
        IntPtr oldTree,
        IntPtr input,
        uint length
    );

    [DllImport(DllName)]
    internal static extern IntPtr ts_parser_parse_string_encoding(
        IntPtr self,
        IntPtr oldTree,
        IntPtr input,
        uint length,
        TsInputEncoding encoding
    );

    [DllImport(DllName)]
    internal static extern void ts_parser_reset(
        IntPtr self
    );

    [DllImport(DllName)]
    internal static extern void ts_parser_set_timeout_micros(
        IntPtr self,
        ulong timeout
    );

    [DllImport(DllName)]
    internal static extern ulong ts_parser_timeout_micros(
        IntPtr self
    );

    [DllImport(DllName)]
    internal static extern void ts_parser_set_cancellation_flag(
        IntPtr self,
        IntPtr flag
    );

    [DllImport(DllName)]
    internal static extern void ts_parser_set_logger(
        IntPtr self,
        TsLogger logger
    );

    [DllImport(DllName)]
    internal static extern TsLogger ts_parser_get_logger(
        IntPtr self
    );

    [DllImport(DllName)]
    internal static extern void ts_parser_print_dot_graphs(
        IntPtr self,
        int file
    );

    #endregion

    #region Tree

    [DllImport(DllName)]
    internal static extern IntPtr ts_tree_copy(
        IntPtr self
    );

    [DllImport(DllName)]
    internal static extern void ts_tree_delete(
        IntPtr self
    );

    [DllImport(DllName)]
    internal static extern TsNode ts_tree_root_node(
        IntPtr self
    );

    [DllImport(DllName)]
    internal static extern IntPtr ts_tree_language(
        IntPtr self
    );

    [DllImport(DllName)]
    internal static extern IntPtr ts_tree_edit(
        IntPtr self,
        ref TsInput edit
    );

    #endregion

    #region Node

    [DllImport(DllName)]
    internal static extern IntPtr ts_node_type(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern ushort ts_node_symbol(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern uint ts_node_start_byte(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern TsPoint ts_node_start_point(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern uint ts_node_end_byte(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern TsPoint ts_node_end_point(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern IntPtr ts_node_string(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern bool ts_node_is_null(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern bool ts_node_is_named(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern bool ts_node_is_missing(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern bool ts_node_is_extra(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern bool ts_node_has_changes(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern bool ts_node_has_error(
        TsNode node
    );

    [DllImport(DllName)]
    [return: MarshalAs(UnmanagedType.Struct)]
    internal static extern TsNode ts_node_parent(
        TsNode node
    );

    [DllImport(DllName)]
    [return: MarshalAs(UnmanagedType.Struct)]
    internal static extern TsNode ts_node_child(
        TsNode node,
        uint index
    );

    [DllImport(DllName)]
    internal static extern uint ts_node_child_count(
        TsNode node
    );

    [DllImport(DllName)]
    [return: MarshalAs(UnmanagedType.Struct)]
    internal static extern TsNode ts_node_named_child(
        TsNode node,
        uint index
    );

    [DllImport(DllName)]
    internal static extern uint ts_node_named_child_count(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern TsNode ts_node_child_by_field_name(
        TsNode node,
        IntPtr fieldName,
        uint fieldNameLength
    );

    [DllImport(DllName)]
    internal static extern TsNode ts_node_child_by_field_id(
        TsNode node,
        ushort fieldId
    );

    [DllImport(DllName)]
    internal static extern TsNode ts_node_next_sibling(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern TsNode ts_node_prev_sibling(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern TsNode ts_node_next_named_sibling(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern TsNode ts_node_prev_named_sibling(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern TsNode ts_node_first_named_child_for_byte(
        TsNode node,
        uint offset
    );

    [DllImport(DllName)]
    internal static extern TsNode ts_node_descendant_for_byte_range(
        TsNode node,
        uint start,
        uint end
    );

    [DllImport(DllName)]
    internal static extern TsNode ts_node_descendant_for_point_range(
        TsNode node,
        TsPoint start,
        TsPoint end
    );

    [DllImport(DllName)]
    internal static extern TsNode ts_node_named_descendant_for_byte_range(
        TsNode node,
        uint start,
        uint end
    );

    [DllImport(DllName)]
    internal static extern TsNode ts_node_named_descendant_for_point_range(
        TsNode node,
        TsPoint start,
        TsPoint end
    );


    [DllImport(DllName)]
    internal static extern void ts_node_edit(
        TsNode node,
        ref TsInputEdit edit
    );


    [DllImport(DllName)]
    internal static extern bool ts_node_eq(
        TsNode node,
        TsNode other
    );

    #endregion

    #region Tree Cursor

    [DllImport(DllName)]
    internal static extern TsTreeCursor ts_tree_cursor_new(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern void ts_tree_cursor_delete(
        ref TsTreeCursor cursor
    );

    [DllImport(DllName)]
    internal static extern TsTreeCursor ts_tree_cursor_reset(
        ref TsTreeCursor self,
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern TsNode ts_tree_cursor_current_node(
        ref TsTreeCursor self
    );

    [DllImport(DllName)]
    internal static extern IntPtr ts_tree_cursor_current_field_name(
        ref TsTreeCursor self
    );

    [DllImport(DllName)]
    internal static extern ushort ts_tree_cursor_current_field_id(
        ref TsTreeCursor self
    );

    [DllImport(DllName)]
    internal static extern bool ts_tree_cursor_goto_parent(
        ref TsTreeCursor self
    );

    [DllImport(DllName)]
    internal static extern bool ts_tree_cursor_goto_next_sibling(
        ref TsTreeCursor self
    );

    [DllImport(DllName)]
    internal static extern bool ts_tree_cursor_goto_first_child(
        ref TsTreeCursor self
    );

    [DllImport(DllName)]
    internal static extern ulong ts_tree_cursor_goto_first_child_for_byte(
        ref TsTreeCursor self,
        uint offset
    );

    [DllImport(DllName)]
    internal static extern TsTreeCursor ts_tree_cursor_copy(
        ref TsTreeCursor self
    );

    #endregion

    #region Language

    [DllImport(DllName)]
    public static extern uint ts_language_symbol_count(
        IntPtr language
    );

    [DllImport(DllName)]
    public static extern IntPtr ts_language_symbol_name(
        IntPtr language,
        ushort symbol
    );

    [DllImport(DllName)]
    public static extern ushort ts_language_symbol_for_name(
        IntPtr language,
        IntPtr name,
        uint length,
        bool isNamed
    );

    [DllImport(DllName)]
    public static extern ushort ts_language_field_count(
        IntPtr language
    );

    [DllImport(DllName)]
    public static extern IntPtr ts_language_field_name_for_id(
        IntPtr language,
        ushort fieldId
    );

    [DllImport(DllName)]
    public static extern ushort ts_language_field_id_for_name(
        IntPtr language,
        IntPtr name,
        uint length
    );

    [DllImport(DllName)]
    public static extern TsSymbolType ts_language_symbol_type(
        IntPtr language,
        ushort symbol
    );

    [DllImport(DllName)]
    public static extern uint ts_language_version(
        IntPtr language
    );

    #endregion
  }
}