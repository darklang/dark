using System;
using System.Collections.Generic;
using System.Dynamic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Xml;
using LibTreeSitter.Native;
using static LibTreeSitter.Native.Native;

namespace LibTreeSitter
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
    /// Get the range of source code that this node represents, both in terms of raw bytes
    /// and of row/column coordinates.
    /// </summary>
    public Range Range => new Range(StartPosition, EndPosition);

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

    /// <summary>
    /// Get this node's number of children.
    /// </summary>
    public int ChildCount => (int)ts_node_child_count(Handle);

    /// <summary>
    /// Iterate over this node's children.
    /// </summary>
    public IEnumerable<Node> Children
    {
      get
      {
        var cursor = new TreeCursor(this);
        cursor.GotoFirstChild();
        return Enumerable.Range(0, ChildCount).Select(_ =>
        {
          var result = cursor.Current;
          cursor.GotoNextSibling();
          return result;
        }).Finally(cursor.Dispose);
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

  public static class EnumerableExtensions
  {
    public static IEnumerable<T> Finally<T>(this IEnumerable<T> enumerable, Action after)
    {
      try
      {
        foreach (var value in enumerable)
          yield return value;
      }
      finally
      {
        after();
      }
    }
  }
}