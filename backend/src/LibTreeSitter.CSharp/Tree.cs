using System;

namespace LibTreeSitter.CSharp
{
  public class Tree : IDisposable
  {
    internal IntPtr _handle;

    internal Tree(IntPtr handle)
    {
      _handle = handle;
    }

    public Node Root =>
      Node.Create(Native.ts_tree_root_node(_handle));

    public void Dispose()
    {
      Native.ts_tree_delete(_handle);
    }
  }
}