using System;
using System.Runtime.InteropServices;
using LibTreeSitter.Native;

namespace LibTreeSitter
{
  public class TreeCursor : IDisposable
  {
    private TsTreeCursor _native;

    internal TreeCursor(Node initial)
    {
      _native = Native.Native.ts_tree_cursor_new(initial.Handle);
    }

    public bool GotoFirstChild()
    {
      return Native.Native.ts_tree_cursor_goto_first_child(ref _native);
    }

    public bool GotoNextSibling()
    {
      return Native.Native.ts_tree_cursor_goto_next_sibling(ref _native);
    }

    public bool GotoParent()
    {
      return Native.Native.ts_tree_cursor_goto_parent(ref _native);
    }

    public Node Current => Node.Create(Native.Native.ts_tree_cursor_current_node(ref _native));

    public string FieldName
    {
      get
      {
        var ptr = Native.Native.ts_tree_cursor_current_field_name(ref _native);
        return ptr == IntPtr.Zero ? null : Marshal.PtrToStringAnsi(ptr);
      }
    }

    public void Dispose()
    {
      Native.Native.ts_tree_cursor_delete(ref _native);
    }
  }
}