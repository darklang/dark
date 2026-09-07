/// Posix libc bridge via P/Invoke — thin wrappers around OS functions.
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


// The libc bridge lives behind the checked host boundary: LibExecution.HostLibc.
module HostLibc = LibExecution.HostLibc
module HostTypes = LibExecution.HostTypes
module Host = LibExecution.Host
module PermissionCheck = LibExecution.PermissionCheck

let private posixErrorTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.Posix.error ())

let private posixErrorTypeRef () = TCustomType(NR.ok (posixErrorTypeName ()), [])

let private posixErrorKT () = KTCustomType(posixErrorTypeName (), [])

/// A failure is the libc (errno, message) pair (errno -1 for a .NET-level
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

/// Map one host outcome to `Result<kt, PosixError>`; `ok` shapes the success.
let private asResult
  (kt : KnownType)
  (outcome : Ply<Result<Host.Response, Host.Failure>>)
  (ok : Host.Response -> Dval)
  : Ply<Dval> =
  uply {
    match! outcome with
    | Ok response -> return Dval.resultOk kt (posixErrorKT ()) (ok response)
    | Error e -> return Dval.resultError kt (posixErrorKT ()) (dPosixError e)
  }

/// Run one posix operation through the checked host boundary and map it to
/// `Result<kt, PosixError>`.
let private posixResult
  (kt : KnownType)
  (state : ExecutionState)
  (vm : VMState)
  (op : HostTypes.PosixOp)
  (ok : Host.Response -> Dval)
  : Ply<Dval> =
  asResult kt (PermissionCheck.performHost state vm (Host.Operation.Posix op)) ok

/// Run one posix lookup whose failure is simply absence: `Option<String>`.
let private posixOption
  (state : ExecutionState)
  (vm : VMState)
  (op : HostTypes.PosixOp)
  (text : Host.Response -> Option<string>)
  : Ply<Dval> =
  uply {
    match! PermissionCheck.performHost state vm (Host.Operation.Posix op) with
    | Ok response ->
      match text response with
      | Some v -> return Dval.optionSome KTString (DString v)
      | None -> return Dval.optionNone KTString
    | Error _ -> return Dval.optionNone KTString
  }

let private unitOk (_ : Host.Response) : Dval = DUnit

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
  [ { name = fn "posixGetcwd" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TypeReference.result TString (posixErrorTypeRef ())
      description = "Returns the current working directory via libc getcwd()"
      fn =
        (function
        | state, vm, _, [| DUnit |] ->
          posixResult KTString state vm HostTypes.PosixOp.Getcwd pathOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.FileRead ]
      deprecated = NotDeprecated }


    { name = fn "posixChdir" 0
      typeParams = []
      parameters = [ Param.make "path" TString "Directory to change to" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Changes the current working directory via libc chdir()"
      fn =
        (function
        | state, vm, _, [| DString path |] ->
          posixResult KTUnit state vm (HostTypes.PosixOp.Chdir path) unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileReadEffect
      deprecated = NotDeprecated }


    { name = fn "posixSetenv" 0
      typeParams = []
      parameters =
        [ Param.make "name" TString "Environment variable name"
          Param.make "value" TString "Value to set" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Sets an environment variable via libc setenv()"
      fn =
        (function
        | state, vm, _, [| DString name; DString value |] ->
          posixResult KTUnit state vm (HostTypes.PosixOp.Setenv(name, value)) unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.EnvWrite ]
      deprecated = NotDeprecated }


    { name = fn "posixUnsetenv" 0
      typeParams = []
      parameters = [ Param.make "name" TString "Environment variable to remove" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Removes an environment variable via libc unsetenv()"
      fn =
        (function
        | state, vm, _, [| DString name |] ->
          posixResult KTUnit state vm (HostTypes.PosixOp.Unsetenv name) unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.EnvWrite ]
      deprecated = NotDeprecated }


    { name = fn "posixMkdir" 0
      typeParams = []
      parameters =
        [ Param.make "path" TString "The directory path to create"
          Param.make "mode" TInt "Permission bits (e.g. 493 for 0755)" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Creates a directory via libc mkdir()"
      fn =
        (function
        | state, vm, _, [| DString path; DInt mode |] ->
          let op = HostTypes.PosixOp.Mkdir(path, intToInt32 vm mode)
          posixResult KTUnit state vm op unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "posixRmdir" 0
      typeParams = []
      parameters = [ Param.make "path" TString "The directory to remove" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Removes an empty directory via libc rmdir()"
      fn =
        (function
        | state, vm, _, [| DString path |] ->
          posixResult KTUnit state vm (HostTypes.PosixOp.Rmdir path) unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "posixUnlink" 0
      typeParams = []
      parameters = [ Param.make "path" TString "The file to remove" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Removes a file via libc unlink()"
      fn =
        (function
        | state, vm, _, [| DString path |] ->
          posixResult KTUnit state vm (HostTypes.PosixOp.Unlink path) unitOk
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
      description = "Renames/moves a file or directory via libc rename()"
      fn =
        (function
        | state, vm, _, [| DString oldpath; DString newpath |] ->
          let op = HostTypes.PosixOp.Rename(oldpath, newpath)
          posixResult KTUnit state vm op unitOk
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
      description = "Changes file permissions via libc chmod()"
      fn =
        (function
        | state, vm, _, [| DString path; DInt mode |] ->
          let op = HostTypes.PosixOp.Chmod(path, intToInt32 vm mode)
          posixResult KTUnit state vm op unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "posixUtimesNow" 0
      typeParams = []
      parameters = [ Param.make "path" TString "File path" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Updates atime and mtime to now via libc utimes(path, NULL)"
      fn =
        (function
        | state, vm, _, [| DString path |] ->
          posixResult KTUnit state vm (HostTypes.PosixOp.UtimesNow path) unitOk
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
      description = "Creates a symbolic link via libc symlink()"
      fn =
        (function
        | state, vm, _, [| DString target; DString linkpath |] ->
          // The target is data stored in the link, not a path opened by this
          // operation; the boundary checks a write at linkpath. Any later
          // access to the link is rejected if it leaves the authorized tree.
          let op = HostTypes.PosixOp.Symlink(target, linkpath)
          posixResult KTUnit state vm op unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "posixReadlink" 0
      typeParams = []
      parameters = [ Param.make "path" TString "Symlink path to read" ]
      returnType = TypeReference.result TString (posixErrorTypeRef ())
      description = "Reads the target of a symbolic link via libc readlink()"
      fn =
        (function
        | state, vm, _, [| DString path |] ->
          posixResult KTString state vm (HostTypes.PosixOp.Readlink path) pathOk
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
        "Creates a unique temp file via libc mkstemp(). Returns (fd, path)."
      fn =
        (function
        | state, vm, _, [| DString prefix |] ->
          posixResult
            (KTTuple(VT.int, VT.string, []))
            state
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
        "Creates a unique temp directory via libc mkdtemp(). Returns the path."
      fn =
        (function
        | state, vm, _, [| DString prefix |] ->
          posixResult KTString state vm (HostTypes.PosixOp.Mkdtemp prefix) pathOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileWriteEffect
      deprecated = NotDeprecated }


    { name = fn "posixListDir" 0
      typeParams = []
      parameters = [ Param.make "path" TString "The directory to list" ]
      returnType = TypeReference.result (TList TString) (posixErrorTypeRef ())
      description =
        "Lists entries in a directory via libc opendir/readdir/closedir. Excludes '.' and '..'."
      fn =
        (function
        | state, vm, _, [| DString path |] ->
          posixResult
            (KTList(ValueType.Known KTString))
            state
            vm
            (HostTypes.PosixOp.ListDir path)
            (fun response ->
              let entries = Host.expectEntries response
              DList(ValueType.Known KTString, List.map DString entries))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileReadEffect
      deprecated = NotDeprecated }


    { name = fn "posixGetenv" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TypeReference.option TString
      description = "Gets an environment variable via libc getenv()"
      fn =
        (function
        | state, vm, _, [| DString name |] ->
          posixOption state vm (HostTypes.PosixOp.Getenv name) Host.expectEnvValue
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
        | state, vm, _, [| DString program; DList(_, args); timeout |] ->
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
          asResult
            processOutcomeKT
            (PermissionCheck.performHost state vm op)
            processOutcomeOk
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
        | state, vm, _, [| DString program; DList(_, args) |] ->
          let argStrs =
            args
            |> List.map (fun d ->
              match d with
              | DString s -> s
              | _ -> incorrectArgs ())
          let op = Host.Operation.ProcessRunInteractive(program, argStrs)
          asResult KTInt (PermissionCheck.performHost state vm op) (fun response ->
            let (exitCode, _, _) = Host.expectProcessOutcome response
            Dval.int (bigint exitCode))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Process ]
      deprecated = NotDeprecated }


    { name = fn "posixKill" 0
      typeParams = []
      parameters =
        [ Param.make "pid" TInt "Process ID"
          Param.make
            "signal"
            TInt
            "Signal number (e.g. 9 for SIGKILL, 15 for SIGTERM)" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Sends a signal to a process."
      fn =
        (function
        | state, vm, _, [| DInt pid; DInt signal |] ->
          let op = HostTypes.PosixOp.Kill(intToInt32 vm pid, intToInt32 vm signal)
          posixResult KTUnit state vm op unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = nativeEffects
      deprecated = NotDeprecated }


    { name = fn "posixFdRead" 0
      typeParams = []
      parameters =
        [ Param.make "fd" TInt "File descriptor to read from"
          Param.make "count" TInt "Max bytes to read" ]
      returnType = TypeReference.result TBlob (posixErrorTypeRef ())
      description =
        "Reads up to count bytes from a file descriptor into an ephemeral Blob."
      fn =
        (function
        | state, vm, _, [| DInt fd; DInt count |] ->
          let op = HostTypes.PosixOp.FdRead(intToInt32 vm fd, intToInt32 vm count)
          posixResult KTBlob state vm op (fun response ->
            Blob.newEphemeral (Host.expectBytes response))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = nativeEffects
      deprecated = NotDeprecated }


    { name = fn "posixFdSeek" 0
      typeParams = []
      parameters =
        [ Param.make "fd" TInt "File descriptor to reposition"
          Param.make "offset" TInt "Byte offset"
          Param.make
            "whence"
            TInt
            "Seek origin: 0 for start, 1 for current position, 2 for end" ]
      returnType = TypeReference.result TInt (posixErrorTypeRef ())
      description =
        "Seeks to a new position in an open file and returns the resulting byte offset."
      fn =
        (function
        | state, vm, _, [| DInt fd; DInt offset; DInt whence |] ->
          let op =
            HostTypes.PosixOp.FdSeek(
              intToInt32 vm fd,
              intToInt64 vm offset,
              intToInt32 vm whence
            )
          posixResult KTInt state vm op (fun response ->
            Dval.int (bigint (Host.expectOffset response)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = nativeEffects
      deprecated = NotDeprecated }


    { name = fn "posixFdWrite" 0
      typeParams = []
      parameters =
        [ Param.make "fd" TInt "File descriptor to write to"
          Param.make "blob" TBlob "Bytes to write" ]
      returnType = TypeReference.result TInt (posixErrorTypeRef ())
      description = "Writes bytes to a file descriptor. Returns bytes written."
      fn =
        (function
        | state, vm, _, [| DInt fd; DBlob ref |] ->
          uply {
            let! bytes = Blob.readBytes state ref
            let op = HostTypes.PosixOp.FdWrite(intToInt32 vm fd, bytes)
            return!
              posixResult KTInt state vm op (fun response ->
                Dval.int (bigint (Host.expectWritten response)))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = nativeEffects
      deprecated = NotDeprecated }


    { name = fn "posixFdClose" 0
      typeParams = []
      parameters = [ Param.make "fd" TInt "File descriptor to close" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Closes a file descriptor."
      fn =
        (function
        | state, vm, _, [| DInt fd |] ->
          let op = HostTypes.PosixOp.FdClose(intToInt32 vm fd)
          posixResult KTUnit state vm op unitOk
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
      description = "Opens a file via libc open(). Returns a file descriptor."
      fn =
        (function
        | state, vm, _, [| DString path; DInt flags; DInt mode |] ->
          let op =
            HostTypes.PosixOp.Open(path, intToInt32 vm flags, intToInt32 vm mode)
          posixResult KTInt state vm op (fun response ->
            Dval.int (bigint (Host.expectFd response)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.union fileEffects nativeEffects
      deprecated = NotDeprecated }


    { name = fn "posixOpenFlag" 0
      typeParams = []
      parameters =
        [ Param.make
            "flag"
            TString
            "One of rdonly, wronly, rdwr, creat, trunc, append" ]
      returnType = TInt
      description = "Returns the platform-specific value of one open() flag, by name"
      fn =
        (function
        | _, _, _, [| DString flag |] ->
          let value =
            match flag with
            | "rdonly" -> HostLibc.O_RDONLY
            | "wronly" -> HostLibc.O_WRONLY
            | "rdwr" -> HostLibc.O_RDWR
            | "creat" -> HostLibc.O_CREAT
            | "trunc" -> HostLibc.O_TRUNC
            | "append" -> HostLibc.O_APPEND
            | other ->
              RuntimeError.UncaughtException($"unknown open() flag `{other}`", [])
              |> raiseUntargetedRTE
          Dval.int (bigint value) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "posixStat" 0
      typeParams = []
      parameters = [ Param.make "path" TString "File path to stat" ]
      returnType =
        TypeReference.result (TTuple(TInt, TInt, [ TInt ])) (posixErrorTypeRef ())
      description = "Stats a file via libc stat(). Returns (mode, size, mtimeSec)."
      fn =
        (function
        | state, vm, _, [| DString path |] ->
          posixResult
            (KTTuple(VT.int, VT.int, [ VT.int ]))
            state
            vm
            (HostTypes.PosixOp.Stat path)
            (fun response ->
              let mode, size, mtimeSec = Host.expectStatInfo response
              DTuple(
                Dval.int (bigint mode),
                Dval.int (bigint size),
                [ Dval.int (bigint mtimeSec) ]
              ))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = fileReadEffect
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
          match HostLibc.uname () with
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


    { name = fn "posixGetpid" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt
      description = "Returns the current process ID via libc getpid()"
      fn =
        (function
        | _, _, _, [| DUnit |] -> Dval.int (bigint (HostLibc.getpid ())) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "posixGetuid" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt
      description = "Returns the current user ID via libc getuid()"
      fn =
        (function
        | _, _, _, [| DUnit |] -> Dval.int (bigint (HostLibc.getuid ())) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "posixGetCurrentUserName" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TypeReference.option TString
      description =
        "Returns the login name of the current user via getuid() + getpwuid()"
      fn =
        (function
        | state, vm, _, [| DUnit |] ->
          let op = HostTypes.PosixOp.UserName(uint32 (HostLibc.getuid ()))
          posixOption state vm op Host.expectOptionalText
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "posixCpuCount" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt
      description = "Returns the number of online CPUs via sysconf()"
      fn =
        (function
        | _, _, _, [| DUnit |] -> Dval.int (bigint (HostLibc.cpuCount ())) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "posixGetHomeDir" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TypeReference.option TString
      description =
        "Returns the home directory of the current user via getpwuid(getuid())"
      fn =
        (function
        | state, vm, _, [| DUnit |] ->
          posixOption state vm HostTypes.PosixOp.HomeDir Host.expectOptionalText
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.FileRead ]
      deprecated = NotDeprecated }


    { name = fn "posixFnmatch" 0
      typeParams = []
      parameters =
        [ Param.make "pattern" TString "Glob pattern"
          Param.make "str" TString "String to match against"
          Param.make "pathMode" TBool "If true, * does not match /" ]
      returnType = TBool
      description = "Matches a string against a glob pattern via libc fnmatch()"
      fn =
        (function
        | _, _, _, [| DString pattern; DString str; DBool pathMode |] ->
          let flags = if pathMode then HostLibc.FNM_PATHNAME else 0
          DBool(HostLibc.fnmatch pattern str flags) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "posixFlock" 0
      typeParams = []
      parameters =
        [ Param.make "fd" TInt "File descriptor"
          Param.make "exclusive" TBool "True for exclusive lock, false to unlock" ]
      returnType = TypeReference.result TUnit (posixErrorTypeRef ())
      description = "Locks or unlocks a file via libc flock()"
      fn =
        (function
        | state, vm, _, [| DInt fd; DBool exclusive |] ->
          let lockOp = if exclusive then HostLibc.LOCK_EX else HostLibc.LOCK_UN
          let op = HostTypes.PosixOp.Flock(intToInt32 vm fd, lockOp)
          posixResult KTUnit state vm op unitOk
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = nativeEffects
      deprecated = NotDeprecated }


    { name = fn "posixFileOwner" 0
      typeParams = []
      parameters = [ Param.make "path" TString "File path" ]
      returnType = TypeReference.result TString (posixErrorTypeRef ())
      description = "Returns the owner username of a file via stat() + getpwuid()"
      fn =
        (function
        | state, vm, _, [| DString path |] ->
          posixResult KTString state vm (HostTypes.PosixOp.FileOwner path) (fun r ->
            DString(Host.expectText r))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.FileRead ]
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
