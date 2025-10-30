module LocalExec.Utils

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes


let isNormalFile (path : string) : bool =
  try
    let attrs = System.IO.File.GetAttributes(path)
    let isDir = attrs.HasFlag(System.IO.FileAttributes.Directory)
    let exists = System.IO.File.Exists(path) || System.IO.Directory.Exists(path)
    exists && not isDir
  with e ->
    false


let rec listDirectoryRecursive (dir : string) : List<string> =
  let contents = System.IO.Directory.EnumerateFileSystemEntries dir |> Seq.toList
  let (files, dirs) = contents |> List.partition (fun x -> isNormalFile x)
  let nested = dirs |> List.map (fun d -> listDirectoryRecursive d) |> List.flatten
  dirs |> List.append files |> List.append nested
