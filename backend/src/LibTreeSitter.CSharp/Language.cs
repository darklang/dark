using System;
using System.Runtime.InteropServices;
using static LibTreeSitter.CSharp.Native.Native;

namespace LibTreeSitter.CSharp
{
  public class Language
  {
    internal IntPtr Handle;

    public Language(IntPtr handle)
    {
      if (handle == IntPtr.Zero)
        throw new ArgumentNullException(nameof(handle));

      Handle = handle;
    }
  }
}