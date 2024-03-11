using System;
using System.Runtime.InteropServices;

namespace LibTreeSitter.CSharp
{
  public class TreeCursor : IDisposable
  {
    private Native.TsTreeCursor _handle;

    internal TreeCursor(Node initial)
    {
      _handle = Native.ts_tree_cursor_new(initial._handle);
    }

    public bool GotoFirstChild()
    {
      return Native.ts_tree_cursor_goto_first_child(ref _handle);
    }

    public bool GotoNextSibling()
    {
      return Native.ts_tree_cursor_goto_next_sibling(ref _handle);
    }

    public bool GotoParent()
    {
      return Native.ts_tree_cursor_goto_parent(ref _handle);
    }

    public Node Current => Node.Create(Native.ts_tree_cursor_current_node(ref _handle));

    public string FieldName
    {
      get
      {
        var ptr = Native.ts_tree_cursor_current_field_name(ref _handle);
        return ptr == IntPtr.Zero ? null : Marshal.PtrToStringAnsi(ptr);
      }
    }

    public void Dispose()
    {
      Native.ts_tree_cursor_delete(ref _handle);
    }
  }
}