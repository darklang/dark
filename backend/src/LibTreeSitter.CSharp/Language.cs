using System;

namespace LibTreeSitter.CSharp
{
  public class Language
  {
    internal IntPtr _handle;

    public Language(IntPtr handle)
    {
      if (handle == IntPtr.Zero)
        throw new ArgumentNullException(nameof(handle));

      _handle = handle;
    }
  }
}