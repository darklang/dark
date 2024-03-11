using System;
using System.Runtime.InteropServices;

namespace LibTreeSitter.CSharp.Native
{
  internal enum TsInputEncoding
  {
    Utf8,
    Utf16
  }

  [StructLayout(LayoutKind.Sequential)]
  internal struct TsPoint
  {
    public uint row;
    public uint column;
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

    // -- Parser
    [DllImport(DllName)]
    internal static extern IntPtr ts_parser_new();

    [DllImport(DllName)]
    internal static extern void ts_parser_delete(IntPtr parser);

    [DllImport(DllName)]
    internal static extern bool ts_parser_set_language(IntPtr self, IntPtr language);

    [DllImport(DllName)]
    internal static extern IntPtr ts_parser_language(IntPtr self);

    [DllImport(DllName)]
    internal static extern IntPtr ts_parser_parse_string_encoding(
        IntPtr self,
        IntPtr oldTree,
        IntPtr input,
        uint length,
        TsInputEncoding encoding
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

    // -- Tree
    [DllImport(DllName)]
    internal static extern void ts_tree_delete(
        IntPtr self
    );

    [DllImport(DllName)]
    internal static extern TsNode ts_tree_root_node(
        IntPtr self
    );

    // - Node
    [DllImport(DllName)]
    internal static extern IntPtr ts_node_type(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern TsPoint ts_node_start_point(
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
    internal static extern bool ts_node_eq(
        TsNode node,
        TsNode other
    );

    // -- Tree cursor
    [DllImport(DllName)]
    internal static extern TsTreeCursor ts_tree_cursor_new(
        TsNode node
    );

    [DllImport(DllName)]
    internal static extern void ts_tree_cursor_delete(
        ref TsTreeCursor cursor
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
  }
}