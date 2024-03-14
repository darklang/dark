module LibTreeSitter.DarklangLanguage

open System
open System.Runtime.InteropServices

module Native =
  open System
  open System.IO
  open System.Reflection
  open System.Runtime.InteropServices

  open LibTreeSitter.Helpers


  // TODO: this should be based on the version of tree-sitter-darklang we're using,
  // or the hash of its grammar.js, or something like that.
  let _treeSitterDarklangVersion = Prelude.randomString 8

  let treeSitterDarklangDirPath =
    Path.Combine(baseTempPath, "tree-sitter-darklang", _treeSitterDarklangVersion)

  if not (Directory.Exists(treeSitterDarklangDirPath)) then
    Directory.CreateDirectory(treeSitterDarklangDirPath) |> ignore<DirectoryInfo>

  // what we're extracting out of our exe
  let resourceName = "tree-sitter-darklang" + resourceExtensionForOS

  // where we're extracting it to
  let resourcePath = Path.Combine(treeSitterDarklangDirPath, resourceName)

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
  type TsDarklang = delegate of unit -> IntPtr

  let libraryHandle : IntPtr = NativeLibrary.Load resourcePath

  let getDelegate (name : string) : 'T =
    Marshal.GetDelegateForFunctionPointer<'T>(
      NativeLibrary.GetExport(libraryHandle, name)
    )

  // Delegate instances
  let tree_sitter_darklang : TsDarklang = getDelegate "tree_sitter_darklang"


let create () = new Language(Native.tree_sitter_darklang.Invoke())
