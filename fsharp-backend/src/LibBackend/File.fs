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
    if value then printfn $"checkFilename failed: {name}: {value}"
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
             && (System.IO.File.GetAttributes(f) <> System.IO.FileAttributes.Normal))
            |> debug "irreg") then
    printfn $"SECURITY_VIOLATION: {f}"
    failwith "FILE SECURITY VIOLATION"
  // FSTODO
  // (Log.erroR "SECURITY_VIOLATION" f
  //  Exception.internal_ "FILE SECURITY VIOLATION")
  else
    f

//
// let file_exists root f : bool =
//   let f = check_filename root Check f in
//   Sys.file_exists f = Yes
//
//
// let mkdir root dir : unit =
//   let dir = check_filename root Dir dir in
//   Unix.mkdir_p dir
//
//
let lsdir (root : Config.Root) (dir : string) : string list =
  let absoluteDir = checkFilename root Dir dir

  absoluteDir
  |> debug "checked"
  |> System.IO.Directory.EnumerateFileSystemEntries
  |> Seq.toList
  |> debug "listed"
  |> List.map (String.dropLeft absoluteDir.Length)
  |> debug "dropped"


// let rm root file : unit =
//   let file = check_filename root Write file in
//   Core_extended.Shell.rm () file


let readfile (root : Config.Root) (f : string) : string =
  f |> checkFilename root Read |> System.IO.File.ReadAllText

// let readfile_lwt root f : string Lwt.t =
//   let f = check_filename root Read f in
//   Lwt_io.with_file mode:Lwt_io.input f Lwt_io.read
//
//
// let writefile root (f : string) (str : string) : unit =
//   let f = check_filename root Write f in
//   let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] in
//   Unix.with_file perm:0600 flags f (fun desc ->
//       ignore (Unix.write desc buf:(Bytes.of_string str)))


(* ------------------- *)
(* json *)
(* ------------------- *)

// let readjsonfile
//     root
//     (stringconv : string -> string = ident)
//     (conv : Yojson.Safe.t -> ('a, string) result)
//     (filename : string) : 'a =
//   filename
//   |> readfile root
//   |> stringconv
//   |> Yojson.Safe.from_string
//   |> conv
//   |> Result.ok_or_failwith
//
//
// let maybereadjsonfile
//     root
//     ?(stringconv : string -> string = ident)
//     (conv : Yojson.Safe.t -> ('a, string) result)
//     (filename : string) : 'a option =
//   if file_exists root filename
//   then Some (readjsonfile root stringconv conv filename)
//   else None
//

(* ------------------- *)
(* spawning *)
(* ------------------- *)
//FSTODO: is this needed for dotnet
// let init () =
//   (* Spawn creates lots of child processes. When they finish, the OS
//    * asks the dark executable what to do. This tells it to ignore them
//    * in such a way that the OS will clean them up. (I thought this was
//    * the default, but this appears to fix the problem). *)
//   Signal.ignore Signal.chld
