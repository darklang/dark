using System;
using System.Runtime.InteropServices;
using static LibTreeSitter.Native.Native;

namespace LibTreeSitter
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