using System;
using System.Collections;
using System.Collections.Generic;
using System.Threading;
using static LibTreeSitter.Native.Native;

namespace LibTreeSitter
{
  public class Tree : IDisposable
  {
    internal IntPtr Handle;

    internal Tree(IntPtr handle)
    {
      Handle = handle;
    }

    public Node Root => Node.Create(ts_tree_root_node(Handle));

    public void Dispose()
    {
      ts_tree_delete(Handle);
    }
  }
}