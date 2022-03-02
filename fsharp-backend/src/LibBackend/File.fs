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

  if (root <> Config.NoCheck)
     && (f.Contains ".." |> debug "dots"
         || f.Contains "~" |> debug "tilde"
         || f.EndsWith "." |> debug "ends dot"
         || (mode <> Dir && f.EndsWith "/") |> debug "ends slash"
         || (not (dir.EndsWith "/")) |> debug "dir no slash"
         || f.EndsWith "etc/passwd" |> debug "etc"
         (* being used wrong *)
         || f.EndsWith "//" |> debug "double slash"
         (* check for irregular file *)
         || (mode = Read
             && (System.IO.File.GetAttributes f <> System.IO.FileAttributes.Normal)
             && (System.IO.File.GetAttributes f <> System.IO.FileAttributes.ReadOnly))
            |> debug "irreg") then
    Exception.raiseInternal "FILE SECURITY VIOLATION" [ "file", f ]
  else
    f


let fileExists root f : bool =
  let f = checkFilename root Check f
  System.IO.File.Exists f

// let mkdir root dir : unit =
//   let dir = check_filename root Dir dir in
//   Unix.mkdir_p dir


let lsdir (root : Config.Root) (dir : string) : string list =
  let absoluteDir = checkFilename root Dir dir

  absoluteDir
  |> System.IO.Directory.EnumerateFileSystemEntries
  |> Seq.toList
  |> List.map (String.dropLeft absoluteDir.Length)


// let rm root file : unit =
//   let file = check_filename root Write file in
//   Core_extended.Shell.rm () file


let readfile (root : Config.Root) (f : string) : string =
  f |> checkFilename root Read |> System.IO.File.ReadAllText

let readfileBytes (root : Config.Root) (f : string) : byte [] =
  f |> checkFilename root Read |> System.IO.File.ReadAllBytes

let tryReadFile (root : Config.Root) (f : string) : string option =
  if fileExists root f then
    f |> checkFilename root Read |> System.IO.File.ReadAllText |> Some
  else
    None

let writefile (root : Config.Root) (f : string) (contents : string) : unit =
  let f = checkFilename root Write f
  System.IO.File.WriteAllText(f, contents)

// let writefile root (f : string) (str : string) : unit =
//   let f = check_filename root Write f in
//   let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] in
//   Unix.with_file perm:0600 flags f (fun desc ->
//       ignore (Unix.write desc buf:(Bytes.of_string str)))
