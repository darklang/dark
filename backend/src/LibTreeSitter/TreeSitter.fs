namespace rec LibTreeSitter

open System
open System.Runtime.InteropServices
open System.Threading
open System.Threading.Tasks


// we need this to make F# OK with the Explicit LayoutKinds
#nowarn "9"

// to make F# ok with the namespace rec
#nowarn "40"

/// Raw I/O against `tree-sitter.so` (or the platform-specific equivalent)
/// TODO: some of these types and functions could be internal
module Native =
  open System
  open System.IO
  open System.Reflection
  open System.Runtime.InteropServices

  open LibTreeSitter.Helpers

  // These bindings correspond to this specific version of tree-sitter
  let private _treeSitterVersion = "v0.20.8"


  type TsInputEncoding =
    | Utf8 = 0
    | Utf16 = 1

  [<Struct; StructLayout(LayoutKind.Sequential)>]
  type TsPoint =
    val mutable row : uint32
    val mutable column : uint32

  // 32 = 4 * 4 + 8 + 8
  [<Struct; StructLayout(LayoutKind.Explicit, Size = 32)>]
  type TsNode =
    // 16 = 4 * 4
    [<FieldOffset(16)>]
    val mutable id : IntPtr

    // 24 = 4 * 4 + 8
    [<FieldOffset(24)>]
    val mutable tree : IntPtr

  // 24 = 8 + 8 + 2 * 4
  [<Struct; StructLayout(LayoutKind.Explicit, Size = 24)>]
  type TsTreeCursor =
    [<FieldOffset(0)>]
    val mutable tree : IntPtr
    [<FieldOffset(8)>]
    val mutable id : IntPtr


  let treeSitterDirPath =
    Path.Combine(baseTempPath, "tree-sitter", _treeSitterVersion)

  if not (Directory.Exists(treeSitterDirPath)) then
    Directory.CreateDirectory(treeSitterDirPath) |> ignore<DirectoryInfo>

  // what we're extracting out of our exe
  let resourceName = "tree-sitter" + resourceExtensionForOS

  // where we're extracting it to
  let resourcePath = Path.Combine(treeSitterDirPath, resourceName)

  // if we haven't previously extracted this version of the library to the temp dir, do so now
  if not (File.Exists resourcePath) then
    let assembly = Assembly.GetExecutingAssembly()
    use stream = assembly.GetManifestResourceStream(resourceName)

    if (stream = null) then
      $"Resource {resourceName} not found in assembly {assembly.FullName}"
      |> Exception
      |> raise
    else
      use fileStream = File.Create(resourcePath)
      stream.CopyTo(fileStream)



  // Function delegates
  type TsParserNew = delegate of unit -> IntPtr
  type TsParserDelete = delegate of IntPtr -> unit
  type TsParserSetLanguage = delegate of IntPtr * IntPtr -> bool
  type TsParserLanguage = delegate of IntPtr -> IntPtr
  type TsParserParseStringEncoding =
    delegate of IntPtr * IntPtr * IntPtr * uint32 * TsInputEncoding -> IntPtr
  type TsParserSetTimeoutMicros = delegate of IntPtr * uint64 -> unit
  type TsParserTimeoutMicros = delegate of IntPtr -> uint64
  type TsParserSetCancellationFlag = delegate of IntPtr * IntPtr -> unit
  type TsTreeDelete = delegate of IntPtr -> unit
  type TsTreeRootNode = delegate of IntPtr -> TsNode
  type TsNodeType = delegate of TsNode -> IntPtr
  type TsNodeStartPoint = delegate of TsNode -> TsPoint
  type TsNodeEndPoint = delegate of TsNode -> TsPoint
  type TsNodeString = delegate of TsNode -> IntPtr
  type TsNodeChildCount = delegate of TsNode -> uint32
  type TsNodeEq = delegate of TsNode * TsNode -> bool
  type TsTreeCursorNew = delegate of TsNode -> TsTreeCursor
  type TsTreeCursorDelete = delegate of TsTreeCursor byref -> unit
  type TsTreeCursorCurrentNode = delegate of TsTreeCursor byref -> TsNode
  type TsTreeCursorCurrentFieldName = delegate of TsTreeCursor byref -> IntPtr
  type TsTreeCursorGotoParent = delegate of TsTreeCursor byref -> bool
  type TsTreeCursorGotoNextSibling = delegate of TsTreeCursor byref -> bool
  type TsTreeCursorGotoFirstChild = delegate of TsTreeCursor byref -> bool


  let libraryHandle : IntPtr = NativeLibrary.Load resourcePath

  let getDelegate (name : string) : 'T =
    Marshal.GetDelegateForFunctionPointer<'T>(
      NativeLibrary.GetExport(libraryHandle, name)
    )

  // Delegate instances
  let ts_parser_new : TsParserNew = getDelegate "ts_parser_new"
  let ts_parser_delete : TsParserDelete = getDelegate "ts_parser_delete"
  let ts_parser_set_language : TsParserSetLanguage =
    getDelegate "ts_parser_set_language"
  let ts_parser_language : TsParserLanguage = getDelegate "ts_parser_language"
  let ts_parser_parse_string_encoding : TsParserParseStringEncoding =
    getDelegate "ts_parser_parse_string_encoding"
  let ts_parser_set_timeout_micros : TsParserSetTimeoutMicros =
    getDelegate "ts_parser_set_timeout_micros"
  let ts_parser_timeout_micros : TsParserTimeoutMicros =
    getDelegate "ts_parser_timeout_micros"
  let ts_parser_set_cancellation_flag : TsParserSetCancellationFlag =
    getDelegate "ts_parser_set_cancellation_flag"
  let ts_tree_delete : TsTreeDelete = getDelegate "ts_tree_delete"
  let ts_tree_root_node : TsTreeRootNode = getDelegate "ts_tree_root_node"
  let ts_node_type : TsNodeType = getDelegate "ts_node_type"
  let ts_node_start_point : TsNodeStartPoint = getDelegate "ts_node_start_point"
  let ts_node_end_point : TsNodeEndPoint = getDelegate "ts_node_end_point"
  let ts_node_string : TsNodeString = getDelegate "ts_node_string"
  let ts_node_child_count : TsNodeChildCount = getDelegate "ts_node_child_count"
  let ts_node_eq : TsNodeEq = getDelegate "ts_node_eq"
  let ts_tree_cursor_new : TsTreeCursorNew = getDelegate "ts_tree_cursor_new"
  let ts_tree_cursor_delete : TsTreeCursorDelete =
    getDelegate "ts_tree_cursor_delete"
  let ts_tree_cursor_current_node : TsTreeCursorCurrentNode =
    getDelegate "ts_tree_cursor_current_node"
  let ts_tree_cursor_current_field_name : TsTreeCursorCurrentFieldName =
    getDelegate "ts_tree_cursor_current_field_name"
  let ts_tree_cursor_goto_parent : TsTreeCursorGotoParent =
    getDelegate "ts_tree_cursor_goto_parent"
  let ts_tree_cursor_goto_next_sibling : TsTreeCursorGotoNextSibling =
    getDelegate "ts_tree_cursor_goto_next_sibling"
  let ts_tree_cursor_goto_first_child : TsTreeCursorGotoFirstChild =
    getDelegate "ts_tree_cursor_goto_first_child"


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
    if node.id = IntPtr.Zero then
      raise (ArgumentNullException "tree-sitter lib returned null node handle")
    else
      Node node

  member _.Handle = handle

  member _.Kind = Marshal.PtrToStringAnsi(Native.ts_node_type.Invoke(handle))

  member this.Range =
    { startPoint = this.StartPosition; endPoint = this.EndPosition }

  member _.StartPosition =
    let res = Native.ts_node_start_point.Invoke(handle)
    { row = int res.row; column = int res.column }

  member _.EndPosition =
    let res = Native.ts_node_end_point.Invoke(handle)
    { row = int res.row; column = int res.column }

  override _.ToString() =
    let cPtr = Native.ts_node_string.Invoke(handle)
    try
      Marshal.PtrToStringAnsi(cPtr)
    finally
      Marshal.FreeHGlobal(cPtr)

  member this.Walk() = new TreeCursor(this)


type TreeCursor(initial : Node) =
  let mutable handle = Native.ts_tree_cursor_new.Invoke(initial.Handle)

  member _.GotoFirstChild() = Native.ts_tree_cursor_goto_first_child.Invoke(&handle)
  member _.GotoNextSibling() =
    Native.ts_tree_cursor_goto_next_sibling.Invoke(&handle)
  member _.GotoParent() = Native.ts_tree_cursor_goto_parent.Invoke(&handle)

  member _.Current = Node.Create(Native.ts_tree_cursor_current_node.Invoke(&handle))

  member _.FieldName =
    let ptr = Native.ts_tree_cursor_current_field_name.Invoke(&handle)
    if ptr = IntPtr.Zero then null else Marshal.PtrToStringAnsi(ptr)

  interface IDisposable with
    member _.Dispose() = Native.ts_tree_cursor_delete.Invoke(&handle)

type Tree(handle : IntPtr) =
  member _.Root = Node.Create(Native.ts_tree_root_node.Invoke(handle))

  interface IDisposable with
    member _.Dispose() = Native.ts_tree_delete.Invoke(handle)

type Parser() =
  let handle = Native.ts_parser_new.Invoke()

  member _.Language
    with get () = Language(Native.ts_parser_language.Invoke(handle))
    and set (value : Language) =
      if not (Native.ts_parser_set_language.Invoke(handle, value.Handle)) then
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
    let cancelFlagPtr = new IntPtr(cancelFlag)

    let encoding =
      match encoding with
      | InputEncoding.Utf16 -> Native.TsInputEncoding.Utf16
      | InputEncoding.Utf8 -> Native.TsInputEncoding.Utf8
      | _ -> Native.TsInputEncoding.Utf8

    if ct.IsSome then
      Native.ts_parser_set_cancellation_flag.Invoke(handle, cancelFlagPtr)
    try
      let resultPtr =
        Native.ts_parser_parse_string_encoding.Invoke(
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
      Native.ts_parser_set_cancellation_flag.Invoke(handle, IntPtr.Zero)

  interface IDisposable with
    member this.Dispose() =
      Native.ts_parser_delete.Invoke(handle)
      GC.SuppressFinalize(this)
