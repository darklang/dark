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

    public int SymbolCount => (int)ts_language_symbol_count(Handle);

    public string SymbolName(ushort symbol) => Marshal.PtrToStringAnsi(ts_language_symbol_name(Handle, symbol));

    public ushort? SymbolForName(string name, bool isNamed)
    {
      var ptr = Marshal.StringToHGlobalAnsi(name);
      var id = ts_language_symbol_for_name(Handle, ptr, (uint)name.Length, isNamed);
      Marshal.FreeHGlobal(ptr);
      return id == 0 ? (ushort?)null : id;
    }

    public int FieldCount => ts_language_field_count(Handle);

    public string FieldNameForId(ushort fieldId) =>
        Marshal.PtrToStringAnsi(ts_language_field_name_for_id(Handle, fieldId));

    public ushort? FieldIdForName(string fieldName)
    {
      var ptr = Marshal.StringToHGlobalAnsi(fieldName);
      var id = ts_language_field_id_for_name(Handle, ptr, (uint)fieldName.Length);
      Marshal.FreeHGlobal(ptr);
      return id == 0 ? (ushort?)null : id;
    }

    public SymbolType SymbolType(ushort symbol) => (SymbolType)ts_language_symbol_type(Handle, symbol);
  }

  public enum SymbolType
  {
    Regular,
    Anonymous,
    Auxiliary
  }
}