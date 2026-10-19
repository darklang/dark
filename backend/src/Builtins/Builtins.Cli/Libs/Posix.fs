/// Typed kernel operations through the checked host boundary.
/// Dark code builds on top of these instead of shelling out to bash.
/// Linux (x86_64, aarch64, armv7) and macOS only. Will not work on Windows.
module Builtins.Cli.Libs.Posix

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution
module Bytes = LibExecution.Bytes
open Builtin.Shortcuts


// The syscall transport lives behind the checked host boundary: LibExecution.HostPosix.
module HostPosix = LibExecution.HostPosix
module HostTypes = LibExecution.HostTypes
module Host = LibExecution.Host
module Interpreter = LibExecution.Interpreter

let private posixErrorTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.Posix.error ())

let private posixErrorTypeRef () = TCustomType(NR.ok (posixErrorTypeName ()), [])

let private posixErrorKT () = KTCustomType(posixErrorTypeName (), [])

/// A failure is the kernel (errno, message) pair (errno -1 for a .NET-level
/// failure), mirroring the Stdlib PosixError type.
let private dPosixError (failure : Host.Failure) : Dval =
  let tn = posixErrorTypeName ()
  DRecord(
    tn,
    tn,
    [],
    Map
      [ "errno", Dval.int (bigint failure.errno)
        "message", DString failure.message ]
  )

/// Name one host operation for the interpreter to perform, its outcome mapped to
/// `Result<kt, PosixError>`; `ok` shapes the success.
let private asResult
  (kt : KnownType)
  (vm : VMState)
  (op : Host.Operation)
  (ok : Host.Response -> Dval)
  : Ply<Dval> =
  Interpreter.requestHost vm op (fun outcome ->
    match outcome with
    | Ok response -> Ply(Dval.resultOk kt (posixErrorKT ()) (ok response))
    | Error e -> Ply(Dval.resultError kt (posixErrorKT ()) (dPosixError e)))

/// Run one posix operation through the checked host boundary and map it to
/// `Result<kt, PosixError>`.
let private posixResult
  (kt : KnownType)
  (vm : VMState)
  (op : HostTypes.PosixOp)
  (ok : Host.Response -> Dval)
  : Ply<Dval> =
  asResult kt vm (Host.Operation.Posix op) ok

/// Run one posix lookup whose failure is simply absence: `Option<String>`.
let private posixOption
  (vm : VMState)
  (op : HostTypes.PosixOp)
  (text : Host.Response -> Option<string>)
  : Ply<Dval> =
  Interpreter.requestHost vm (Host.Operation.Posix op) (fun outcome ->
    match outcome with
    | Ok response ->
      match text response with
      | Some v -> Ply(Dval.optionSome KTString (DString v))
      | None -> Ply(Dval.optionNone KTString)
    | Error _ -> Ply(Dval.optionNone KTString))

let private unitOk (_ : Host.Response) : Dval = DUnit

/// One kernel operation. Errno is data; rendering and retries belong in Dark.
let private syscallResult
  (kt : KnownType)
  (vm : VMState)
  (op : HostTypes.PosixOp)
  (ok : Host.Response -> Dval)
  : Ply<Dval> =
  Interpreter.requestHost vm (Host.Operation.Posix op) (fun outcome ->
    match outcome with
    | Ok response -> Ply(Dval.resultOk kt KTInt (ok response))
    | Error e -> Ply(Dval.resultError kt KTInt (Dval.int (bigint e.errno))))

let private fileEffects = set [ Effect.FileRead; Effect.FileWrite ]

let private fileReadEffect = set [ Effect.FileRead ]
let private fileWriteEffect = set [ Effect.FileWrite ]

/// Raw descriptors, pids and spawns: the operation names a number, not a
/// resource, so it is granted whole or not at all. Host facts (uname, pid,
/// uid, cpu count) are effect-free, and an answer that is a path (cwd, home,
/// a file's owner) is a read of that path, checked at the boundary.
let private nativeEffects = set [ Effect.Native ]

let fns () : List<BuiltInFn> =
  [ { name = fn "runtimeTargetOS" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "The execution target OS, not the compiler build host"
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          let os =
            if System.OperatingSystem.IsBrowser() then "browser"
            elif System.OperatingSystem.IsMacOS() then "darwin"
            elif System.OperatingSystem.IsWindows() then "windows"
            else "linux"
          Ply(DString os)
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "runtimeTargetArchitecture" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "The execution target architecture"
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          let arch =
            System.Runtime.InteropServices.RuntimeInformation.ProcessArchitecture
          Ply(DString(arch.ToString().ToLowerInvariant()))
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "syscallStatAt" 0
      typeParams = []
      parameters = [ Param.make "path" TString "Checked, no-follow entry" ]
      returnType = TypeReference.result TBytes TInt
      description = "Raw kernel stat bytes; field decoding belongs in Dark"
      fn =
        (function
        | _, vm, _, [| DString path |] ->
          syscallResult KTBytes vm (HostTypes.PosixOp.StatBuffer path) (fun r ->
            Bytes.newEphemeral (Host.expectBytes r))
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = fileReadEffect
      deprecated = NotDeprecated }

    { name = fn "syscallGetdents" 0
      typeParams = []
      parameters =
        [ Param.make "fd" TInt "Directory descriptor"
          Param.make "count" TInt "Maximum raw bytes" ]
      returnType = TypeReference.result TBytes TInt
      description = "One getdents64/getdirentries64 call; no record decoding"
      fn =
        (function
        | _, vm, _, [| DInt fd; DInt count |] ->
          let op =
            HostTypes.PosixOp.DirectoryBytes(
              intToDescriptor vm fd,
              intToInt32 vm count
            )
          syscallResult KTBytes vm op (fun r ->
            Bytes.newEphemeral (Host.expectBytes r))
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "syscallGetcwd" 0
      typeParams = []
      parameters = [ Param.make "count" TInt "Maximum raw bytes" ]
      returnType = TypeReference.result TBytes TInt
      description = "One kernel getcwd call; no buffer growth or public decoding"
      fn =
        (function
        | _, vm, _, [| DInt count |] ->
          syscallResult
            KTBytes
            vm
            (HostTypes.PosixOp.GetcwdBytes(intToInt32 vm count))
            (fun r -> Bytes.newEphemeral (Host.expectBytes r))
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.FileRead ]
      deprecated = NotDeprecated }


    { name = fn "syscallChdir" 0
      typeParams = []
      parameters = [ Param.make "path" TString "Directory to change to" ]
      returnType = TypeReference.result TUnit TInt
      description = "Changes the current working directory via kernel chdir()"
      fn =
        (function
        | _, vm, _, [| DString path |] ->
          syscallResult KTUnit vm (HostTypes.PosixOp.Chdir path) unitOk
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = fileReadEffect
      deprecated = NotDeprecated }




    { name = fn "syscallMkdir" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString "The directory path to create"
          Param.make "mode" TInt "Permission bits (e.g. 493 for 0755)" ]
      returnType = TypeReference.result TUnit TInt
      description = "Creates a directory via kernel mkdir()"
      fn =
        (function
        | _, vm, _, [| DString path; DInt mode |] ->
          let op = HostTypes.PosixOp.Mkdir(path, intToInt32 vm mode)
          syscallResult KTUnit vm op unitOk
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "syscallUnlinkAt" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString "The entry to remove"
          Param.make "flags" TInt "Kernel unlinkat flags" ]
      returnType = TypeReference.result TUnit TInt
      description = "One checked entry-relative unlinkat call"
      fn =
        (function
        | _, vm, _, [| DString path; DInt flags |] ->
          syscallResult
            KTUnit
            vm
            (HostTypes.PosixOp.UnlinkAt(path, intToInt32 vm flags))
            unitOk
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "syscallRename" 0
      typeParams = []
      parameters =
        [ Param.make "oldpath" TString "Current path"
          Param.make "newpath" TString "New path" ]
      returnType = TypeReference.result TUnit TInt
      description = "Renames/moves a file or directory via kernel rename()"
      fn =
        (function
        | _, vm, _, [| DString oldpath; DString newpath |] ->
          let op = HostTypes.PosixOp.Rename(oldpath, newpath)
          syscallResult KTUnit vm op unitOk
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "syscallChmod" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString "File path"
          Param.make "mode" TInt "Permission bits (e.g. 493 for 0755)" ]
      returnType = TypeReference.result TUnit TInt
      description = "Changes file permissions via kernel chmod()"
      fn =
        (function
        | _, vm, _, [| DString path; DInt mode |] ->
          let op = HostTypes.PosixOp.Chmod(path, intToInt32 vm mode)
          syscallResult KTUnit vm op unitOk
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "syscallSymlink" 0
      typeParams = []
      parameters =
        [ Param.make "target" TString "Path the symlink points to"
          Param.make "linkpath" TString "Path of the symlink to create" ]
      returnType = TypeReference.result TUnit TInt
      description = "Creates a symbolic link via kernel symlink()"
      fn =
        (function
        | _, vm, _, [| DString target; DString linkpath |] ->
          // The target is data stored in the link, not a path opened by this
          // operation; the boundary checks a write at linkpath. Any later
          // access to the link is rejected if it leaves the authorized tree.
          let op = HostTypes.PosixOp.Symlink(target, linkpath)
          syscallResult KTUnit vm op unitOk
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "syscallReadlinkAt" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString "Symlink path to read"
          Param.make "count" TInt "Maximum bytes" ]
      returnType = TypeReference.result TBytes TInt
      description = "One readlinkat call, with no growth or text decoding"
      fn =
        (function
        | _, vm, _, [| DString path; DInt count |] ->
          syscallResult
            KTBytes
            vm
            (HostTypes.PosixOp.ReadlinkBytes(path, intToInt32 vm count))
            (fun response -> Bytes.newEphemeral (Host.expectBytes response))
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = fileReadEffect
      deprecated = NotDeprecated }
















    { name = fn "syscallKill" 0
      typeParams = []
      parameters =
        [ Param.make "pid" TInt "Process ID"
          Param.make
            "signal"
            TInt
            "Signal number (e.g. 9 for SIGKILL, 15 for SIGTERM)" ]
      returnType = TypeReference.result TUnit TInt
      description = "Sends a signal to a process."
      fn =
        (function
        | _, vm, _, [| DInt pid; DInt signal |] ->
          let op = HostTypes.PosixOp.Kill(intToInt32 vm pid, intToInt32 vm signal)
          syscallResult KTUnit vm op unitOk
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = nativeEffects
      deprecated = NotDeprecated }


    { name = fn "syscallRead" 0
      typeParams = []
      parameters =
        [ Param.make "fd" TInt "File descriptor to read from"
          Param.make "count" TInt "Max bytes to read" ]
      returnType = TypeReference.result TBytes TInt
      description =
        "Reads up to count bytes from a file descriptor into an ephemeral Bytes."
      fn =
        (function
        | _, vm, _, [| DInt fd; DInt count |] ->
          let op =
            HostTypes.PosixOp.FdRead(intToDescriptor vm fd, intToInt32 vm count)
          syscallResult KTBytes vm op (fun response ->
            Bytes.newEphemeral (Host.expectBytes response))
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "syscallLseek" 0
      typeParams = []
      parameters =
        [ Param.make "fd" TInt "File descriptor to reposition"
          Param.make "offset" TInt "Byte offset"
          Param.make
            "whence"
            TInt
            "Seek origin: 0 for start, 1 for current position, 2 for end" ]
      returnType = TypeReference.result TInt TInt
      description =
        "Seeks to a new position in an open file and returns the resulting byte offset."
      fn =
        (function
        | _, vm, _, [| DInt fd; DInt offset; DInt whence |] ->
          let op =
            HostTypes.PosixOp.FdSeek(
              intToDescriptor vm fd,
              intToInt64 vm offset,
              intToInt32 vm whence
            )
          syscallResult KTInt vm op (fun response ->
            Dval.int (bigint (Host.expectOffset response)))
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "syscallWrite" 0
      typeParams = []
      parameters =
        [ Param.make "fd" TInt "File descriptor to write to"
          Param.make "bytes" TBytes "Bytes to write" ]
      returnType = TypeReference.result TInt TInt
      description = "Writes bytes to a file descriptor. Returns bytes written."
      fn =
        (function
        | state, vm, _, [| DInt fd; DBytes ref |] ->
          Bytes.withBytes state ref (fun bytes ->
            let op = HostTypes.PosixOp.FdWrite(intToDescriptor vm fd, bytes)
            syscallResult KTInt vm op (fun response ->
              Dval.int (bigint (Host.expectWritten response))))
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "syscallClose" 0
      typeParams = []
      parameters = [ Param.make "fd" TInt "File descriptor to close" ]
      returnType = TypeReference.result TUnit TInt
      description = "Closes a file descriptor."
      fn =
        (function
        | _, vm, _, [| DInt fd |] ->
          let op = HostTypes.PosixOp.FdClose(intToDescriptor vm fd)
          syscallResult KTUnit vm op unitOk
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "syscallOpen" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString "File path to open"
          Param.make "flags" TInt "Open flags (e.g. O_RDONLY, O_WRONLY | O_CREAT)"
          Param.make "mode" TInt "Permission bits for new files (e.g. 420 for 0644)" ]
      returnType = TypeReference.result TInt TInt
      description = "Opens a file via kernel open(). Returns a file descriptor."
      fn =
        (function
        | _, vm, _, [| DString path; DInt flags; DInt mode |] ->
          let op =
            HostTypes.PosixOp.Open(path, intToInt32 vm flags, intToInt32 vm mode)
          syscallResult KTInt vm op (fun response ->
            Dval.int (Host.expectDescriptor response))
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = fileEffects
      deprecated = NotDeprecated }









    { name = fn "syscallGetpid" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt
      description = "Returns the current process ID via kernel getpid()"
      fn =
        (function
        | _, _, _, [| DUnit |] -> Dval.int (bigint (HostPosix.getpid ())) |> Ply
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "syscallGetuid" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt
      description = "Returns the current user ID via kernel getuid()"
      fn =
        (function
        | _, _, _, [| DUnit |] -> Dval.int (bigint (HostPosix.getuid ())) |> Ply
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }






    { name = fn "runtimeAuthorizeReadPath" 0
      typeParams = []
      parameters = [ Param.make "home" TString "Candidate home directory" ]
      returnType = TypeReference.option TString
      description = "Authorizes a home directory candidate selected in Darklang"
      fn =
        (function
        | _, vm, _, [| DString home |] ->
          posixOption vm (HostTypes.PosixOp.HomeDir home) Host.expectOptionalText
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.FileRead ]
      deprecated = NotDeprecated }





    { name = fn "syscallFlock" 0
      typeParams = []
      parameters =
        [ Param.make "fd" TInt "File descriptor"
          Param.make "operation" TInt "Kernel LOCK_* operation bits" ]
      returnType = TypeReference.result TUnit TInt
      description = "Locks or unlocks a file via kernel flock()"
      fn =
        (function
        | _, vm, _, [| DInt fd; DInt operation |] ->
          let lockOp = intToInt32 vm operation
          let op = HostTypes.PosixOp.Flock(intToDescriptor vm fd, lockOp)
          syscallResult KTUnit vm op unitOk
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    ]


let builtins () : Builtins = Builtin.make [] (fns ())
