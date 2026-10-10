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
module Blob = LibExecution.Blob
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
    | Error e ->
      Ply(Dval.resultError kt KTInt (Dval.int (bigint e.errno))))

let private pathOk (response : Host.Response) : Dval =
  DString(Host.expectPath response)

let private fileEffects = set [ Effect.FileRead; Effect.FileWrite ]

let private fileReadEffect = set [ Effect.FileRead ]
let private fileWriteEffect = set [ Effect.FileWrite ]

/// Raw descriptors, pids and spawns: the operation names a number, not a
/// resource, so it is granted whole or not at all. Host facts (uname, pid,
/// uid, cpu count) are effect-free, and an answer that is a path (cwd, home,
/// a file's owner) is a read of that path, checked at the boundary.
let private nativeEffects = set [ Effect.Native ]

let private processOutcomeKT = KTTuple(VT.int, VT.string, [ VT.string ])

let private processOutcomeOk (response : Host.Response) : Dval =
  let exitCode, stdout, stderr = Host.expectProcessOutcome response
  DTuple(Dval.int (bigint exitCode), DString stdout, [ DString stderr ])

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
      sqlSpec = NotQueryable
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
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "syscallStatAt" 0
      typeParams = []
      parameters = [ Param.make "path" TString "Checked, no-follow entry" ]
      returnType = TypeReference.result TBlob TInt
      description = "Raw kernel stat bytes; field decoding belongs in Dark"
      fn =
        (function
        | _, vm, _, [| DString path |] ->
          syscallResult KTBlob vm (HostTypes.PosixOp.StatBuffer path) (fun r ->
            Blob.newEphemeral (Host.expectBytes r))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileReadEffect
      deprecated = NotDeprecated }

    { name = fn "syscallGetdents" 0
      typeParams = []
      parameters =
        [ Param.make "fd" TInt "Directory descriptor"
          Param.make "count" TInt "Maximum raw bytes" ]
      returnType = TypeReference.result TBlob TInt
      description = "One getdents64/getdirentries64 call; no record decoding"
      fn =
        (function
        | _, vm, _, [| DInt fd; DInt count |] ->
          let op =
            HostTypes.PosixOp.DirectoryBytes(intToInt32 vm fd, intToInt32 vm count)
          syscallResult KTBlob vm op (fun r ->
            Blob.newEphemeral (Host.expectBytes r))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = nativeEffects
      deprecated = NotDeprecated }

    { name = fn "posixGetcwd" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TypeReference.result TString (posixErrorTypeRef ())
      description = "Returns the current working directory via kernel getcwd()"
      fn =
        (function
        | _, vm, _, [| DUnit |] ->
          posixResult KTString vm HostTypes.PosixOp.Getcwd pathOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.FileRead ]
      deprecated = NotDeprecated }


    { name = fn "posixChdir" 0
      typeParams = []
      parameters = [ Param.make "path" TString "Directory to change to" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Changes the current working directory via kernel chdir()"
      fn =
        (function
        | _, vm, _, [| DString path |] ->
          posixResult KTUnit vm (HostTypes.PosixOp.Chdir path) unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileReadEffect
      deprecated = NotDeprecated }




    { name = fn "posixMkdir" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString "The directory path to create"
          Param.make "mode" TInt "Permission bits (e.g. 493 for 0755)" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Creates a directory via kernel mkdir()"
      fn =
        (function
        | _, vm, _, [| DString path; DInt mode |] ->
          let op = HostTypes.PosixOp.Mkdir(path, intToInt32 vm mode)
          posixResult KTUnit vm op unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "posixRmdir" 0
      typeParams = []
      parameters = [ Param.make "path" TString "The directory to remove" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Removes an empty directory via kernel rmdir()"
      fn =
        (function
        | _, vm, _, [| DString path |] ->
          posixResult KTUnit vm (HostTypes.PosixOp.Rmdir path) unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "posixUnlink" 0
      typeParams = []
      parameters = [ Param.make "path" TString "The file to remove" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Removes a file via kernel unlink()"
      fn =
        (function
        | _, vm, _, [| DString path |] ->
          posixResult KTUnit vm (HostTypes.PosixOp.Unlink path) unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "posixRename" 0
      typeParams = []
      parameters =
        [ Param.make "oldpath" TString "Current path"
          Param.make "newpath" TString "New path" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Renames/moves a file or directory via kernel rename()"
      fn =
        (function
        | _, vm, _, [| DString oldpath; DString newpath |] ->
          let op = HostTypes.PosixOp.Rename(oldpath, newpath)
          posixResult KTUnit vm op unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "posixChmod" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString "File path"
          Param.make "mode" TInt "Permission bits (e.g. 493 for 0755)" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Changes file permissions via kernel chmod()"
      fn =
        (function
        | _, vm, _, [| DString path; DInt mode |] ->
          let op = HostTypes.PosixOp.Chmod(path, intToInt32 vm mode)
          posixResult KTUnit vm op unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "posixUtimesNow" 0
      typeParams = []
      parameters = [ Param.make "path" TString "File path" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Updates atime and mtime to now via kernel utimes(path, NULL)"
      fn =
        (function
        | _, vm, _, [| DString path |] ->
          posixResult KTUnit vm (HostTypes.PosixOp.UtimesNow path) unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "posixSymlink" 0
      typeParams = []
      parameters =
        [ Param.make "target" TString "Path the symlink points to"
          Param.make "linkpath" TString "Path of the symlink to create" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Creates a symbolic link via kernel symlink()"
      fn =
        (function
        | _, vm, _, [| DString target; DString linkpath |] ->
          // The target is data stored in the link, not a path opened by this
          // operation; the boundary checks a write at linkpath. Any later
          // access to the link is rejected if it leaves the authorized tree.
          let op = HostTypes.PosixOp.Symlink(target, linkpath)
          posixResult KTUnit vm op unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "posixReadlink" 0
      typeParams = []
      parameters = [ Param.make "path" TString "Symlink path to read" ]
      returnType = TypeReference.result TString (posixErrorTypeRef ())
      description = "Reads the target of a symbolic link via kernel readlink()"
      fn =
        (function
        | _, vm, _, [| DString path |] ->
          posixResult KTString vm (HostTypes.PosixOp.Readlink path) pathOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileReadEffect
      deprecated = NotDeprecated }


    { name = fn "posixMkstemp" 0
      typeParams = []
      parameters =
        [ Param.make
            "prefix"
            TString
            "Prefix for the temp file path (e.g. \"/tmp/dark-\")" ]
      returnType =
        TypeReference.result (TTuple(TInt, TString, [])) (posixErrorTypeRef ())
      description =
        "Creates a unique temp file via kernel mkstemp(). Returns (fd, path)."
      fn =
        (function
        | _, vm, _, [| DString prefix |] ->
          posixResult
            (KTTuple(VT.int, VT.string, []))
            vm
            (HostTypes.PosixOp.Mkstemp prefix)
            (fun response ->
              let fd, path = Host.expectFdPath response
              DTuple(Dval.int (bigint fd), DString path, []))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.union fileWriteEffect nativeEffects
      deprecated = NotDeprecated }


    { name = fn "posixMkdtemp" 0
      typeParams = []
      parameters =
        [ Param.make
            "prefix"
            TString
            "Prefix for the temp directory path (e.g. \"/tmp/dark-\")" ]
      returnType = TypeReference.result TString (posixErrorTypeRef ())
      description =
        "Creates a unique temp directory via kernel mkdtemp(). Returns the path."
      fn =
        (function
        | _, vm, _, [| DString prefix |] ->
          posixResult KTString vm (HostTypes.PosixOp.Mkdtemp prefix) pathOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }





    { name = fn "posixGetenv" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TypeReference.option TString
      description = "Gets an environment variable via kernel getenv()"
      fn =
        (function
        | _, vm, _, [| DString name |] ->
          posixOption vm (HostTypes.PosixOp.Getenv name) Host.expectEnvValue
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.EnvRead ]
      deprecated = NotDeprecated }


    { name = fn "posixSpawnAndWait" 0
      typeParams = []
      parameters =
        [ Param.make "program" TString "Path to the executable"
          Param.make "args" (TList TString) "Arguments to pass"
          Param.make
            "timeoutMs"
            (TypeReference.option TInt)
            "Kill the child and fail with ETIMEDOUT after this many milliseconds; None waits" ]
      returnType =
        TypeReference.result
          (TTuple(TInt, TString, [ TString ]))
          (posixErrorTypeRef ())
      description =
        "Spawns a child process, waits for it to finish, returns (exitCode, stdout, stderr)."
      fn =
        (function
        | _, vm, _, [| DString program; DList(_, args); timeout |] ->
          let argStrs =
            args
            |> List.map (fun d ->
              match d with
              | DString s -> s
              | _ -> incorrectArgs ())
          let timeoutMs =
            match timeout with
            | DEnum(_, _, _, "Some", [ DInt ms ]) -> Some(intToInt32 vm ms)
            | DEnum(_, _, _, "None", []) -> None
            | _ -> incorrectArgs ()
          let op = Host.Operation.ProcessRun(program, argStrs, timeoutMs)
          asResult processOutcomeKT vm op processOutcomeOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Process ]
      deprecated = NotDeprecated }


    { name = fn "posixSpawnInteractive" 0
      typeParams = []
      parameters =
        [ Param.make "program" TString "Path to the executable"
          Param.make "args" (TList TString) "Arguments to pass" ]
      returnType = TypeReference.result TInt (posixErrorTypeRef ())
      description =
        "Runs a child process on this terminal, inheriting stdin/stdout/stderr, and "
        + "returns its exit code. For programs that DRAW, like an editor: capturing "
        + "their output would leave them painting into a pipe."
      fn =
        (function
        | _, vm, _, [| DString program; DList(_, args) |] ->
          let argStrs =
            args
            |> List.map (fun d ->
              match d with
              | DString s -> s
              | _ -> incorrectArgs ())
          let op = Host.Operation.ProcessRunInteractive(program, argStrs)
          asResult KTInt vm op (fun response ->
            let (exitCode, _, _) = Host.expectProcessOutcome response
            Dval.int (bigint exitCode))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Process ]
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
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = nativeEffects
      deprecated = NotDeprecated }


    { name = fn "syscallRead" 0
      typeParams = []
      parameters =
        [ Param.make "fd" TInt "File descriptor to read from"
          Param.make "count" TInt "Max bytes to read" ]
      returnType = TypeReference.result TBlob TInt
      description =
        "Reads up to count bytes from a file descriptor into an ephemeral Blob."
      fn =
        (function
        | _, vm, _, [| DInt fd; DInt count |] ->
          let op = HostTypes.PosixOp.FdRead(intToInt32 vm fd, intToInt32 vm count)
          syscallResult KTBlob vm op (fun response ->
            Blob.newEphemeral (Host.expectBytes response))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = nativeEffects
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
              intToInt32 vm fd,
              intToInt64 vm offset,
              intToInt32 vm whence
            )
          syscallResult KTInt vm op (fun response ->
            Dval.int (bigint (Host.expectOffset response)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = nativeEffects
      deprecated = NotDeprecated }


    { name = fn "syscallWrite" 0
      typeParams = []
      parameters =
        [ Param.make "fd" TInt "File descriptor to write to"
          Param.make "blob" TBlob "Bytes to write" ]
      returnType = TypeReference.result TInt TInt
      description = "Writes bytes to a file descriptor. Returns bytes written."
      fn =
        (function
        | state, vm, _, [| DInt fd; DBlob ref |] ->
          Blob.withBytes state ref (fun bytes ->
            let op = HostTypes.PosixOp.FdWrite(intToInt32 vm fd, bytes)
            syscallResult KTInt vm op (fun response ->
              Dval.int (bigint (Host.expectWritten response))))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = nativeEffects
      deprecated = NotDeprecated }


    { name = fn "syscallClose" 0
      typeParams = []
      parameters = [ Param.make "fd" TInt "File descriptor to close" ]
      returnType = TypeReference.result TUnit TInt
      description = "Closes a file descriptor."
      fn =
        (function
        | _, vm, _, [| DInt fd |] ->
          let op = HostTypes.PosixOp.FdClose(intToInt32 vm fd)
          syscallResult KTUnit vm op unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = nativeEffects
      deprecated = NotDeprecated }


    { name = fn "posixOpen" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString "File path to open"
          Param.make "flags" TInt "Open flags (e.g. O_RDONLY, O_WRONLY | O_CREAT)"
          Param.make "mode" TInt "Permission bits for new files (e.g. 420 for 0644)" ]
      returnType = TypeReference.result TInt (posixErrorTypeRef ())
      description = "Opens a file via kernel open(). Returns a file descriptor."
      fn =
        (function
        | _, vm, _, [| DString path; DInt flags; DInt mode |] ->
          let op =
            HostTypes.PosixOp.Open(path, intToInt32 vm flags, intToInt32 vm mode)
          posixResult KTInt vm op (fun response ->
            Dval.int (bigint (Host.expectFd response)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.union fileEffects nativeEffects
      deprecated = NotDeprecated }








    { name = fn "posixUname" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TypeReference.result
          (TTuple(TString, TString, [ TString ]))
          (posixErrorTypeRef ())
      description = "Calls uname(). Returns (sysname, nodename, machine)."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          let kt = KTTuple(VT.string, VT.string, [ VT.string ])
          match HostPosix.uname () with
          | Ok(sysname, nodename, machine) ->
            DTuple(DString sysname, DString nodename, [ DString machine ])
            |> Dval.resultOk kt (posixErrorKT ())
            |> Ply
          | Error e ->
            dPosixError (Host.failureOfErrno e)
            |> Dval.resultError kt (posixErrorKT ())
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
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
      sqlSpec = NotQueryable
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
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "posixGetCurrentUserName" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TypeReference.option TString
      description = "Returns the process-managed login name of the current user"
      fn =
        (function
        | _, vm, _, [| DUnit |] ->
          let op = HostTypes.PosixOp.UserName(uint32 (HostPosix.getuid ()))
          posixOption vm op Host.expectOptionalText
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "posixCpuCount" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt
      description = "Returns the number of CPUs available to the interpreter"
      fn =
        (function
        | _, _, _, [| DUnit |] -> Dval.int (bigint (HostPosix.cpuCount ())) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "posixCheckHomeDir" 0
      typeParams = []
      parameters = [ Param.make "home" TString "Candidate home directory" ]
      returnType = TypeReference.option TString
      description = "Authorizes a home directory candidate selected in Darklang"
      fn =
        (function
        | _, vm, _, [| DString home |] ->
          posixOption vm (HostTypes.PosixOp.HomeDir home) Host.expectOptionalText
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.FileRead ]
      deprecated = NotDeprecated }


    { name = fn "posixGetHomeDir" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TypeReference.option TString
      description =
        "Returns the framework account home directory after authorizing that path"
      fn =
        (function
        | _, vm, _, [| DUnit |] ->
          posixOption vm HostTypes.PosixOp.DefaultHomeDir Host.expectOptionalText
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
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
          let op = HostTypes.PosixOp.Flock(intToInt32 vm fd, lockOp)
          syscallResult KTUnit vm op unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = nativeEffects
      deprecated = NotDeprecated }


 ]


let builtins () : Builtins = Builtin.make [] (fns ())
