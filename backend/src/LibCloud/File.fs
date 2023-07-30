/// Helpers to work with files on disk
module LibBackend.File

// This makes extra careful that we're only accessing files where we expect to
// find files, and that we're not checking outside these directories

// Note: none of these are async because System.IO is not async

open Prelude
open Tablecloth

type Mode =
  | Check
  | Dir
  | Read
  | Write

let checkFilename (root : Config.Root) (mode : Mode) (f : string) =
  let dir = Config.dir root
  let f : string = $"{dir}{f}"

  let debug (name : string) (value : bool) =
    if value then print $"checkFilename failed: {name}: {value}"
    value

  if
    (root <> Config.NoCheck)
    && (f.Contains ".." |> debug "dots"
        || f.Contains "~" |> debug "tilde"
        || f.EndsWith "." |> debug "ends dot"
        || (mode <> Dir && f.EndsWith "/") |> debug "ends slash"
        || (not (dir.EndsWith "/")) |> debug "dir no slash"
        || f.EndsWith "etc/passwd" |> debug "etc"
        // being used wrong
        || f.EndsWith "//" |> debug "double slash"
        // check for irregular file
        || (mode = Read
            && (System.IO.File.GetAttributes f <> System.IO.FileAttributes.Normal)
            && (System.IO.File.GetAttributes f <> System.IO.FileAttributes.ReadOnly))
           |> debug "irreg")
  then
    Exception.raiseInternal "FILE SECURITY VIOLATION" [ "file", f ]
  else
    f


let fileExists root f : bool =
  let f = checkFilename root Check f
  System.IO.File.Exists f

let lsdir (root : Config.Root) (dir : string) : string list =
  let absoluteDir = checkFilename root Dir dir

  absoluteDir
  |> System.IO.Directory.EnumerateFileSystemEntries
  |> Seq.toList
  |> List.map (String.dropLeft absoluteDir.Length)

let lsPattern (root : Config.Root) (pattern : string) : string list =
  let absoluteDir = checkFilename root Dir ""

  System.IO.Directory.EnumerateFileSystemEntries(absoluteDir, pattern)
  |> Seq.toList
  |> List.map (String.dropLeft absoluteDir.Length)


let readfile (root : Config.Root) (f : string) : string =
  f |> checkFilename root Read |> System.IO.File.ReadAllText

let readfileBytes (root : Config.Root) (f : string) : byte[] =
  f |> checkFilename root Read |> System.IO.File.ReadAllBytes

let tryReadFile (root : Config.Root) (f : string) : string option =
  if fileExists root f then
    f |> checkFilename root Read |> System.IO.File.ReadAllText |> Some
  else
    None

let rec writefileBytes (root : Config.Root) (f : string) (contents : byte[]) : unit =
  let f = checkFilename root Write f

  // First write to a temp file, then copy atomically. Do this as we've lost our data
  // a few times.
  let tempFilename = System.IO.Path.GetTempFileName()
  System.IO.File.WriteAllBytes(tempFilename, contents)

  // We might not be the only one trying to copy here, and .NET won't let us
  // overwrite it if something else is. So try again.
  let mutable success = false
  let mutable count = 0
  while success = false && count < 10 do
    try
      System.IO.File.Move(tempFilename, f, true)
      success <- true
    with e ->
      count <- count + 1
      if count > 10 then e.Reraise() else ()
      ()

let rec writefile (root : Config.Root) (f : string) (contents : string) : unit =
  writefileBytes root f (UTF8.toBytes contents)
