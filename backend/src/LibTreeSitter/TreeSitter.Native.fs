/// Raw F# bindings (P/Invoke) for tree-sitter
/// TODO: some of these types and functions could be internal
[<RequireQualifiedAccess>]
module LibTreeSitter.Native

open System
open System.IO
open System.Reflection
open System.Runtime.InteropServices

// we need this to make F# OK with the Explicit LayoutKinds
#nowarn "9"


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


// this is its own fn because we'll use it also for the tree-sitter-darklang library
let baseTempPath = Path.Combine(Path.GetTempPath(), "darklang")

let treeSitterDirPath = Path.Combine(baseTempPath, "tree-sitter", _treeSitterVersion)

if not (Directory.Exists(treeSitterDirPath)) then
  Directory.CreateDirectory(treeSitterDirPath) |> ignore<DirectoryInfo>

let resourceExtensionForOS =
  if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then ".dll"
  elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then ".so"
  elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then ".dylib"
  else raise (PlatformNotSupportedException())

// what we're extracting out of our exe
let resourceName = "tree-sitter" + resourceExtensionForOS

// where we're extracting it to
let resourcePath = Path.Combine(treeSitterDirPath, resourceName)

// if we haven't previously extracted this version of the library to the temp dir, do so now
if not (File.Exists resourcePath) then
  Console.WriteLine($"Extracting tree-sitter native library to {resourcePath}")

  let assembly = Assembly.GetExecutingAssembly()
  use stream = assembly.GetManifestResourceStream(resourceName)

  if (stream = null) then
    $"Resource {resourceName} not found in assembly {assembly.FullName}"
    |> Exception
    |> raise
  else
    use fileStream = File.Create(resourcePath)
    stream.CopyTo(fileStream)
else
  Console.WriteLine($"Tree-sitter native library already exists at {resourcePath}")



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
let ts_tree_cursor_delete : TsTreeCursorDelete = getDelegate "ts_tree_cursor_delete"
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
