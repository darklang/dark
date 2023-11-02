using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Win32.SafeHandles;
using LibTreeSitter.Native;
using static LibTreeSitter.Native.Native;

namespace LibTreeSitter
{
  public class Parser : IDisposable
  {
    private readonly IntPtr _handle;
    private LogCallback _logCallback = null;
    private SafeFileHandle _dotFileHandle = null;

    /// <summary>
    /// Create new parser.
    /// </summary>
    public Parser()
    {
      _handle = ts_parser_new();
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
        if (!ts_parser_set_language(_handle, value.Handle))
          throw new TreeSitterException("Could not set language");
      }
      get => new Language(ts_parser_language(_handle));
    }

    /// <summary>
    /// Use the parser to parse some source code and create a syntax tree.
    /// </summary>
    /// <param name="text">source text to parse</param>
    /// <param name="oldTree">old version of the document, or <c>null</c> if this is the first time parsing it</param>
    /// <param name="token">cancellation token to use</param>
    /// <returns>the parsed syntax tree</returns>
    public Tree Parse(string text, Tree oldTree = null, CancellationToken? token = null)
    {
      var ptr = Marshal.StringToHGlobalUni(text);
      try
      {
        return ParseInternal(ptr, (uint)text.Length * 2, InputEncoding.Utf16, oldTree, token);
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
    /// <param name="oldTree">old version of the document, or <c>null</c> if this is the first time parsing it</param>
    /// <param name="token">cancellation token to use</param>
    /// <returns>the parsed syntax tree</returns>
    public unsafe Tree Parse(byte[] bytes, InputEncoding encoding, Tree oldTree = null,
        CancellationToken? token = null)
    {
      fixed (byte* ptr = bytes)
      {
        return ParseInternal(new IntPtr(ptr), (uint)bytes.LongLength, encoding, oldTree, token);
      }
    }

    private unsafe Tree ParseInternal(IntPtr input, uint length, InputEncoding encoding, Tree oldTree, CancellationToken? token)
    {
      var cancelFlag = 0L;
      CancellationTokenRegistration? registration = null;

      if (token is CancellationToken ct)
      {
        var cancelFlagPtr = &cancelFlag;
        ts_parser_set_cancellation_flag(_handle, new IntPtr(cancelFlagPtr));
        registration = ct.Register(() => *cancelFlagPtr = 1);
      }

      try
      {
        var result = ts_parser_parse_string_encoding(
            _handle,
            oldTree?.Handle ?? IntPtr.Zero,
            input,
            length,
            (TsInputEncoding)encoding
        );

        if (result == IntPtr.Zero)
          throw new TaskCanceledException("parsing canceled");

        return new Tree(result);
      }
      finally
      {
        ts_parser_set_cancellation_flag(_handle, IntPtr.Zero);
        registration?.Dispose();
      }
    }

    public LogCallback Logger
    {
      get => _logCallback;
      set
      {
        _logCallback = value;
        ts_parser_set_logger(_handle, new TsLogger
        {
          log = InternalLogHandler,
          payload = IntPtr.Zero,
        });
      }
    }

    private void InternalLogHandler(IntPtr payload, TsLogType logtype, IntPtr data)
    {
      var type = (LogType)logtype;
      var arg = Marshal.PtrToStringAnsi(data);
      _logCallback(type, arg);
    }

    /// <summary>
    /// Set the file descriptor to which the parser should write debugging graphs
    /// during parsing. The graphs are formatted in the DOT language. You may want
    /// to pipe these graphs directly to a `dot(1)` process in order to generate
    /// SVG output. You can turn off this logging by passing <c>null</c>
    /// </summary>
    /// <param name="fs">file stream to write to, or <c>null</c> to turn off</param>
    /// <exception cref="ArgumentException">if getting safe file handle failed</exception>
    /// <exception cref="Exception">unsafe operations failed</exception>
    public void PrintDotGraphs(FileStream fs)
    {
      _dotFileHandle?.DangerousRelease();
      _dotFileHandle = null;

      if (fs is null)
      {
        ts_parser_print_dot_graphs(_handle, -1);
      }
      else
      {
        var handle = fs.SafeFileHandle ?? throw new ArgumentException("could not get file handle");
        var result = false;
        handle.DangerousAddRef(ref result);
        if (!result)
          throw new Exception("could not increment handle count");
        _dotFileHandle = handle;
        ts_parser_print_dot_graphs(_handle, (int)handle.DangerousGetHandle());
      }
    }

    /// <summary>
    /// Instruct the parser to start the next parse from the beginning.
    /// <para>
    /// If the parser previously failed because of a timeout or a cancellation, then
    /// by default, it will resume where it left off on the next call to
    /// <see cref="Parse(string,TreeSitter.Tree)"/> or other parsing functions. If you don't want to resume,
    /// and instead intend to use this parser to parse some other document, you must
    /// call <see cref="Reset"/> first.
    /// </para>
    /// </summary>
    public void Reset()
    {
      ts_parser_reset(_handle);
    }

    /// <summary>
    /// Get or set the maximum duration in microseconds that parsing should be allowed to
    /// take before halting.
    /// </summary>
    public TimeSpan Timeout
    {
      get => TimeSpan.FromMilliseconds(ts_parser_timeout_micros(_handle) / 1000.0);
      set => ts_parser_set_timeout_micros(_handle, (ulong)(value.TotalMilliseconds * 1000));
    }

    /// <summary>
    /// Delete the parser, freeing all of the memory that it used.
    /// </summary>
    public void Dispose()
    {
      ts_parser_delete(_handle);
      GC.SuppressFinalize(this);
    }

    ~Parser()
    {
      ts_parser_delete(_handle);
    }
  }

  public enum InputEncoding
  {
    Utf8,
    Utf16
  }

  public delegate void LogCallback(LogType type, string arg);

  public enum LogType
  {
    Parse,
    Lex
  }
}