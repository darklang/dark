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
module Bytes = LibExecution.Bytes
open Builtin.Shortcuts


// The syscall transport lives behind the checked host boundary: LibExecution.HostPosix.
module HostPosix = LibExecution.HostPosix
module HostTypes = LibExecution.HostTypes
module Host = LibExecution.Host
module Interpreter = LibExecution.Interpreter

/// Run one posix lookup whose failure is simply absence: `Option<String>`.
let private posixOption
  (vm : VMState)
  (op : HostTypes.PosixOp)
  (text : Host.Response -> Option<string>)
  : Dval =
  Interpreter.requestHost vm (Host.Operation.Posix op) (fun outcome ->
    match outcome with
    | Ok response ->
      match text response with
      | Some v -> (Dval.optionSome KTString (DString v))
      | None -> (Dval.optionNone KTString)
    | Error _ -> (Dval.optionNone KTString))

let private unitOk (_ : Host.Response) : Dval = DUnit

/// One kernel operation. Errno is data; rendering and retries belong in Dark.
let private syscallResult
  (kt : KnownType)
  (vm : VMState)
  (op : HostTypes.PosixOp)
  (ok : Host.Response -> Dval)
  : Dval =
  Interpreter.requestHost vm (Host.Operation.Posix op) (fun outcome ->
    match outcome with
    | Ok response -> (Dval.resultOk kt KTInt64 (ok response))
    | Error e -> (Dval.resultError kt KTInt64 (Dval.dint64 (int64 e.errno))))

let private fileEffects = set [ Effect.FileRead; Effect.FileWrite ]

let private fileReadEffect = set [ Effect.FileRead ]
let private fileWriteEffect = set [ Effect.FileWrite ]

/// Raw descriptors, pids and spawns: the operation names a number, not a
/// resource, so it is granted whole or not at all. Host facts (uname, pid,
/// uid, cpu count) are effect-free, and an answer that is a path (cwd, home,
/// a file's owner) is a read of that path, checked at the boundary.
let private nativeEffects = set [ Effect.Native ]

/// What an `extern … from "kernel"` declaration in the Posix package calls.
let kernel (symbol : string) : FQFnName.Builtin =
  let provider = LibExecution.ProgramTypes.Expr.externProvider "kernel" symbol
  { name = provider.name; version = provider.version }

/// The provider for one `extern … from "kernel"` in the Posix package. The extern declares the
/// signature, and its own frame checks the arguments and the result, so the provider only says
/// how many arguments it takes, what it does, and its effects.
let provider
  (symbol : string)
  (arity : int)
  (effects : Set<Effect>)
  (body : ExecutionState -> VMState -> Dval[] -> Dval)
  : BuiltInFn =
  { name = kernel symbol
    typeParams = []
    parameters =
      List.init arity (fun i -> Param.make $"arg{i}" (TVariable $"a{i}") "")
    returnType = TVariable "result"
    description = "A kernel call; the extern in Stdlib.Cli.Posix declares it"
    fn = (fun struct (state, vm, _, values) -> body state vm values)
    previewable = Impure
    callEffects = effects
    deprecated = NotDeprecated }

let private bytes (r : Host.Response) = Bytes.newEphemeral (Host.expectBytes r)

let private raises = set [ Effect.Raise ]

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
          (DString os)
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
          (DString(arch.ToString().ToLowerInvariant()))
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    provider "statAt" 1 fileReadEffect (fun _ vm values ->
      match values with
      | [| DString path |] ->
        syscallResult KTBytes vm (HostTypes.PosixOp.StatBuffer path) bytes
      | _ -> incorrectArgs ())

    provider "getdents" 2 raises (fun _ vm values ->
      match values with
      | [| DBigInt fd; DInt64 count |] ->
        let op =
          HostTypes.PosixOp.DirectoryBytes(
            bigIntToDescriptor fd,
            int64ToInt32 count
          )
        syscallResult KTBytes vm op bytes
      | _ -> incorrectArgs ())

    provider "getcwd" 1 raises (fun _ vm values ->
      match values with
      | [| DInt64 count |] ->
        let op = HostTypes.PosixOp.GetcwdBytes(int64ToInt32 count)
        syscallResult KTBytes vm op bytes
      | _ -> incorrectArgs ())

    provider "chdir" 1 fileReadEffect (fun _ vm values ->
      match values with
      | [| DString path |] ->
        syscallResult KTUnit vm (HostTypes.PosixOp.Chdir path) unitOk
      | _ -> incorrectArgs ())

    provider "mkdir" 2 (Set.add Effect.Raise fileWriteEffect) (fun _ vm values ->
      match values with
      | [| DString path; DInt64 mode |] ->
        let op = HostTypes.PosixOp.Mkdir(path, int64ToInt32 mode)
        syscallResult KTUnit vm op unitOk
      | _ -> incorrectArgs ())

    provider "unlinkAt" 2 (Set.add Effect.Raise fileWriteEffect) (fun _ vm values ->
      match values with
      | [| DString path; DInt64 flags |] ->
        let op = HostTypes.PosixOp.UnlinkAt(path, int64ToInt32 flags)
        syscallResult KTUnit vm op unitOk
      | _ -> incorrectArgs ())

    provider "rename" 2 fileWriteEffect (fun _ vm values ->
      match values with
      | [| DString oldpath; DString newpath |] ->
        let op = HostTypes.PosixOp.Rename(oldpath, newpath)
        syscallResult KTUnit vm op unitOk
      | _ -> incorrectArgs ())

    provider "chmod" 2 (Set.add Effect.Raise fileWriteEffect) (fun _ vm values ->
      match values with
      | [| DString path; DInt64 mode |] ->
        let op = HostTypes.PosixOp.Chmod(path, int64ToInt32 mode)
        syscallResult KTUnit vm op unitOk
      | _ -> incorrectArgs ())

    provider "symlink" 2 fileWriteEffect (fun _ vm values ->
      match values with
      | [| DString target; DString linkpath |] ->
        // The target is data stored in the link, not a path opened by this
        // operation; the boundary checks a write at linkpath. Any later
        // access to the link is rejected if it leaves the authorized tree.
        let op = HostTypes.PosixOp.Symlink(target, linkpath)
        syscallResult KTUnit vm op unitOk
      | _ -> incorrectArgs ())

    provider "readlinkAt" 2 (Set.add Effect.Raise fileReadEffect) (fun _ vm values ->
      match values with
      | [| DString path; DInt64 count |] ->
        let op = HostTypes.PosixOp.ReadlinkBytes(path, int64ToInt32 count)
        syscallResult KTBytes vm op bytes
      | _ -> incorrectArgs ())

    provider "kill" 2 (Set.add Effect.Raise nativeEffects) (fun _ vm values ->
      match values with
      | [| DInt64 pid; DInt64 signal |] ->
        let op = HostTypes.PosixOp.Kill(int64ToInt32 pid, int64ToInt32 signal)
        syscallResult KTUnit vm op unitOk
      | _ -> incorrectArgs ())

    provider "read" 2 raises (fun _ vm values ->
      match values with
      | [| DBigInt fd; DInt64 count |] ->
        let op = HostTypes.PosixOp.FdRead(bigIntToDescriptor fd, int64ToInt32 count)
        syscallResult KTBytes vm op bytes
      | _ -> incorrectArgs ())

    provider "lseek" 3 raises (fun _ vm values ->
      match values with
      | [| DBigInt fd; DInt64 offset; DInt64 whence |] ->
        let op =
          HostTypes.PosixOp.FdSeek(
            bigIntToDescriptor fd,
            offset,
            int64ToInt32 whence
          )
        syscallResult KTInt64 vm op (fun response ->
          Dval.dint64 (int64 (Host.expectOffset response)))
      | _ -> incorrectArgs ())

    provider "write" 2 raises (fun state vm values ->
      match values with
      | [| DBigInt fd; DBytes ref |] ->
        Bytes.withBytes state ref (fun bytes ->
          let op = HostTypes.PosixOp.FdWrite(bigIntToDescriptor fd, bytes)
          syscallResult KTInt64 vm op (fun response ->
            Dval.dint64 (int64 (Host.expectWritten response))))
      | _ -> incorrectArgs ())

    provider "close" 1 raises (fun _ vm values ->
      match values with
      | [| DBigInt fd |] ->
        let op = HostTypes.PosixOp.FdClose(bigIntToDescriptor fd)
        syscallResult KTUnit vm op unitOk
      | _ -> incorrectArgs ())

    provider "open" 3 (Set.add Effect.Raise fileEffects) (fun _ vm values ->
      match values with
      | [| DString path; DInt64 flags; DInt64 mode |] ->
        let op = HostTypes.PosixOp.Open(path, int64ToInt32 flags, int64ToInt32 mode)
        syscallResult KTBigInt vm op (fun response ->
          Dval.bigInt (Host.expectDescriptor response))
      | _ -> incorrectArgs ())

    provider "getpid" 1 Set.empty (fun _ _ values ->
      match values with
      | [| DUnit |] -> Dval.dint64 (int64 (HostPosix.getpid ()))
      | _ -> incorrectArgs ())

    provider "getuid" 1 Set.empty (fun _ _ values ->
      match values with
      | [| DUnit |] -> Dval.dint64 (int64 (HostPosix.getuid ()))
      | _ -> incorrectArgs ())

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

    provider "flock" 2 raises (fun _ vm values ->
      match values with
      | [| DBigInt fd; DInt64 operation |] ->
        let op =
          HostTypes.PosixOp.Flock(bigIntToDescriptor fd, int64ToInt32 operation)
        syscallResult KTUnit vm op unitOk
      | _ -> incorrectArgs ()) ]


let builtins () : Builtins = Builtin.make [] (fns ())
