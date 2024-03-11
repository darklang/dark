using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using LibTreeSitter.CSharp.Native;
using static LibTreeSitter.CSharp.Native.Native;

namespace LibTreeSitter.CSharp
{
  public class Node
  {
    internal TsNode Handle;

    internal static Node Create(TsNode node)
    {
      return node.id == IntPtr.Zero ? null : new Node(node);
    }

    private Node(TsNode node)
    {
      Handle = node;
    }

    /// <summary>
    /// Get this node's type as a string
    /// </summary>
    public string Kind => Marshal.PtrToStringAnsi(ts_node_type(Handle));

    /// <summary>
    /// Get this node's start position in terms of rows and columns.
    /// </summary>
    public Point StartPosition
    {
      get
      {
        var res = ts_node_start_point(Handle);
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
        var res = ts_node_end_point(Handle);
        return new Point((int)res.row, (int)res.column);
      }
    }

    /// <summary> S-expression representation of the node. </summary>
    public override string ToString()
    {
      var cPtr = ts_node_string(Handle);
      var result = Marshal.PtrToStringAnsi(cPtr);

      // Free the memory
      Marshal.FreeHGlobal(cPtr);

      return result;
    }

    public TreeCursor Walk()
    {
      return new TreeCursor(this);
    }

    protected bool Equals(Node other)
    {
      return ts_node_eq(Handle, other.Handle);
    }

    public override bool Equals(object obj)
    {
      if (ReferenceEquals(null, obj)) return false;
      if (ReferenceEquals(this, obj)) return true;
      if (obj.GetType() != this.GetType()) return false;
      return Equals((Node)obj);
    }

    public override int GetHashCode()
    {
      return Handle.id.ToInt32();
    }
  }
}