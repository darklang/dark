module rec LibTreeSitter.Main

open System
open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks

type TreeSitterException(message : string) =
  inherit Exception(message)

type InputEncoding =
  | Utf8 = 0
  | Utf16 = 1

type Point = { row : int; column : int }
type Range = { startPoint : Point; endPoint : Point }


type Language(handle : IntPtr) =
  do if handle = IntPtr.Zero then raise (ArgumentNullException "handle")
  member _.Handle = handle


type Node(handle : Native.TsNode) =
  static member internal Create(node : Native.TsNode) =
    //if node.id = IntPtr.Zero then null else
    Node(node)

  member _.Handle = handle

  member _.Kind = Marshal.PtrToStringAnsi(Native.ts_node_type.Invoke (handle))

  member this.Range =
    { startPoint = this.StartPosition; endPoint = this.EndPosition }

  member _.StartPosition =
    let res = Native.ts_node_start_point.Invoke (handle)
    { row = int res.row; column = int res.column }

  member _.EndPosition =
    let res = Native.ts_node_end_point.Invoke (handle)
    { row = int res.row; column = int res.column }

  override _.ToString() =
    let cPtr = Native.ts_node_string.Invoke (handle)
    try
      Marshal.PtrToStringAnsi(cPtr)
    finally
      Marshal.FreeHGlobal(cPtr)

  member this.Walk() = new TreeCursor(this)


type TreeCursor(initial : Node) =
  let mutable handle = Native.ts_tree_cursor_new.Invoke (initial.Handle)

  member _.GotoFirstChild() = Native.ts_tree_cursor_goto_first_child.Invoke (&handle)
  member _.GotoNextSibling() = Native.ts_tree_cursor_goto_next_sibling.Invoke (&handle)
  member _.GotoParent() = Native.ts_tree_cursor_goto_parent.Invoke (&handle)

  member _.Current = Node.Create(Native.ts_tree_cursor_current_node.Invoke (&handle))

  member _.FieldName =
    let ptr = Native.ts_tree_cursor_current_field_name.Invoke (&handle)
    if ptr = IntPtr.Zero then null else Marshal.PtrToStringAnsi(ptr)

  interface IDisposable with
    member _.Dispose() = Native.ts_tree_cursor_delete.Invoke (&handle)

type Tree(handle : IntPtr) =
  member _.Root = Node.Create(Native.ts_tree_root_node.Invoke (handle))

  interface IDisposable with
    member _.Dispose() = Native.ts_tree_delete.Invoke (handle)

type Parser() =
  let handle = Native.ts_parser_new.Invoke ()

  member _.Language
    with get () = Language(Native.ts_parser_language.Invoke (handle))
    and set (value : Language) =
      if not (Native.ts_parser_set_language.Invoke (handle, value.Handle)) then
        raise (TreeSitterException("Could not set language"))

  member this.Parse(text : string, ct : Option<CancellationToken>) =
    let bytes = System.Text.Encoding.UTF8.GetBytes(text)
    this.Parse(bytes, InputEncoding.Utf8, ct)

  member this.Parse
    (
      bytes : byte[],
      encoding : InputEncoding,
      ct : Option<CancellationToken>
    ) =
    let length = uint32 (bytes.Length)
    let gch = GCHandle.Alloc(bytes, GCHandleType.Pinned)
    try
      let ptr = gch.AddrOfPinnedObject()
      this.ParseInternal(ptr, length, encoding, ct)
    finally
      gch.Free()

  member private _.ParseInternal
    (
      input : IntPtr,
      length : uint32,
      encoding : InputEncoding,
      ct : Option<CancellationToken>
    ) =
    let mutable cancelFlag = 0L
    let cancelFlagPtr = new IntPtr(cancelFlag) // had & -- ok?

    let encoding =
      match encoding with
      | InputEncoding.Utf16 -> Native.TsInputEncoding.Utf16
      | InputEncoding.Utf8 -> Native.TsInputEncoding.Utf8
      | _ -> Native.TsInputEncoding.Utf8

    if ct.IsSome then
      Native.ts_parser_set_cancellation_flag.Invoke (handle, cancelFlagPtr)
    try
      let resultPtr =
        Native.ts_parser_parse_string_encoding.Invoke (
          handle,
          IntPtr.Zero,
          input,
          length,
          encoding
        )
      if resultPtr = IntPtr.Zero then
        raise (TaskCanceledException("Parsing canceled"))
      new Tree(resultPtr)
    finally
      Native.ts_parser_set_cancellation_flag.Invoke (handle, IntPtr.Zero)

  interface IDisposable with
    member this.Dispose() =
      Native.ts_parser_delete.Invoke (handle)
      GC.SuppressFinalize(this)
