using System;
using System.Runtime.InteropServices;
using System.Threading;
using System.Threading.Tasks;

namespace LibTreeSitter.CSharp
{
  public class Parser : IDisposable
  {
    private readonly IntPtr _handle;

    /// <summary>
    /// Create new parser.
    /// </summary>
    public Parser()
    {
      _handle = Native.ts_parser_new();
    }

    /// <summary>
    /// Get or set the language that the parser should use for parsing
    /// </summary>
    /// <exception cref="TreeSitterException">
    /// Thrown when the language is not successfully set.
    /// The cause is most likely version mismatch.
    /// </exception>
    public Language Language
    {
      set
      {
        if (!Native.ts_parser_set_language(_handle, value._handle))
          throw new TreeSitterException("Could not set language");
      }
      get => new Language(Native.ts_parser_language(_handle));
    }

    /// <summary>
    /// Use the parser to parse some source code and create a syntax tree.
    /// </summary>
    /// <param name="text">source text to parse</param>
    /// <param name="token">cancellation token to use</param>
    /// <returns>the parsed syntax tree</returns>
    public Tree Parse(string text, CancellationToken? token = null)
    {
      var ptr = Marshal.StringToHGlobalUni(text);
      try
      {
        return ParseInternal(ptr, (uint)text.Length * 2, InputEncoding.Utf16, token);
      }
      finally
      {
        Marshal.FreeHGlobal(ptr);
      }
    }

    /// <summary>
    /// Use the parser to parse some source code and create a syntax tree.
    /// </summary>
    /// <param name="bytes">source text to parse</param>
    /// <param name="encoding">text encoding</param>
    /// <param name="token">cancellation token to use</param>
    /// <returns>the parsed syntax tree</returns>
    public unsafe Tree Parse(byte[] bytes, InputEncoding encoding,
        CancellationToken? token = null)
    {
      fixed (byte* ptr = bytes)
      {
        return ParseInternal(new IntPtr(ptr), (uint)bytes.LongLength, encoding, token);
      }
    }

    private unsafe Tree ParseInternal(IntPtr input, uint length, InputEncoding encoding, CancellationToken? token)
    {
      var cancelFlag = 0L;
      CancellationTokenRegistration? registration = null;

      if (token is CancellationToken ct)
      {
        var cancelFlagPtr = &cancelFlag;
        Native.ts_parser_set_cancellation_flag(_handle, new IntPtr(cancelFlagPtr));
        registration = ct.Register(() => *cancelFlagPtr = 1);
      }

      try
      {
        var result = Native.ts_parser_parse_string_encoding(
            _handle,
            IntPtr.Zero,
            input,
            length,
            (Native.TsInputEncoding)encoding
        );

        if (result == IntPtr.Zero)
          throw new TaskCanceledException("parsing canceled");

        return new Tree(result);
      }
      finally
      {
        Native.ts_parser_set_cancellation_flag(_handle, IntPtr.Zero);
        registration?.Dispose();
      }
    }

    /// <summary>
    /// Get or set the maximum duration in microseconds that parsing should be allowed to
    /// take before halting.
    /// </summary>
    public TimeSpan Timeout
    {
      get => TimeSpan.FromMilliseconds(Native.ts_parser_timeout_micros(_handle) / 1000.0);
      set => Native.ts_parser_set_timeout_micros(_handle, (ulong)(value.TotalMilliseconds * 1000));
    }

    /// <summary>
    /// Delete the parser, freeing all of the memory that it used.
    /// </summary>
    public void Dispose()
    {
      Native.ts_parser_delete(_handle);
      GC.SuppressFinalize(this);
    }

    ~Parser()
    {
      Native.ts_parser_delete(_handle);
    }
  }

  public enum InputEncoding
  {
    Utf8,
    Utf16
  }
}