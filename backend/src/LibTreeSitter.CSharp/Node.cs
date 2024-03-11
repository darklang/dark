using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;

namespace LibTreeSitter.CSharp
{
  public class Node
  {
    internal Native.TsNode _handle;

    internal static Node Create(Native.TsNode node)
    {
      return node.id == IntPtr.Zero ? null : new Node(node);
    }

    private Node(Native.TsNode node)
    {
      _handle = node;
    }

    /// <summary>
    /// Get this node's type as a string
    /// </summary>
    public string Kind => Marshal.PtrToStringAnsi(Native.ts_node_type(_handle));

    /// <summary>
    /// Get this node's start position in terms of rows and columns.
    /// </summary>
    public Point StartPosition
    {
      get
      {
        var res = Native.ts_node_start_point(_handle);
        return new Point((int)res.row, (int)res.column);
      }
    }

    /// <summary>
    /// Get this node's end position in terms of rows and columns.
    /// </summary>
    public Point EndPosition
    {
      get
      {
        var res = Native.ts_node_end_point(_handle);
        return new Point((int)res.row, (int)res.column);
      }
    }

    /// <summary> S-expression representation of the node. </summary>
    public override string ToString()
    {
      var cPtr = Native.ts_node_string(_handle);
      var result = Marshal.PtrToStringAnsi(cPtr);

      // Free the memory
      Marshal.FreeHGlobal(cPtr);

      return result;
    }

    public TreeCursor Walk()
    {
      return new TreeCursor(this);
    }
  }
}