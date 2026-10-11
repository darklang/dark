/// Single kernel operations. Dark libraries own retries and all algorithms.
module Builtins.Cli.Libs.Syscall

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module Blob = LibExecution.Blob
module Host = LibExecution.Host
module Op = LibExecution.HostTypes
module Interpreter = LibExecution.Interpreter
module Builtin = LibExecution.Builtin

let private perform kt vm op shape =
  Interpreter.requestHost vm (Host.Operation.Posix op) (fun result ->
    match result with
    | Ok response -> Ply(Dval.resultOk kt KTInt (shape response))
    | Error e -> Ply(Dval.resultError kt KTInt (Dval.int (bigint e.errno))))

let private make name parameters result _kt body : BuiltInFn =
  { name = fn name 0
    typeParams = []
    parameters = parameters
    returnType = TypeReference.result result TInt
    description = "One kernel operation; numeric errno, no retries or decoding"
    fn = (fun struct (state, vm, _, values) -> body state vm values)
    sqlSpec = NotQueryable
    previewable = Impure
    // Conditional file/Native authorization is derived from the descriptor
    // by Host. No ambient Native gate may reject a scoped file capability.
    callEffects = Set.empty
    deprecated = NotDeprecated }

let private integer name = Param.make name TInt "Kernel scalar"
let private blob name = Param.make name TBlob "Bytes"
let private bytes r = Blob.newEphemeral (Host.expectBytes r)
let private unit (_ : Host.Response) = DUnit
let private offset r = Dval.int (bigint (Host.expectOffset r))
let private written r = Dval.int (bigint (Host.expectWritten r))

let private timestamp name constructor : BuiltInFn =
  { (make name
      [ Param.make "path" TString "Checked path"
        integer "accessSeconds"; integer "accessNanos"
        integer "modifySeconds"; integer "modifyNanos" ] TUnit KTUnit
      (fun _ vm values ->
        match values with
        | [| DString path; DInt a; DInt an; DInt m; DInt mn |] ->
          perform KTUnit vm (constructor(path, intToInt64 vm a, intToInt64 vm an,
            intToInt64 vm m, intToInt64 vm mn)) unit
        | _ -> incorrectArgs ()))
      with callEffects = set [ Effect.FileWrite ] }

let fns () : List<BuiltInFn> =
  [ timestamp "syscallUtimensAt" Op.PosixOp.UtimensAt
    timestamp "syscallSetattrlistTimes" Op.PosixOp.SetattrlistTimes
    make "syscallUname" [ Param.make "unit" TUnit "" ] TBlob KTBlob
      (fun _ vm values ->
        match values with
        | [| DUnit |] -> perform KTBlob vm Op.PosixOp.UnameBytes bytes
        | _ -> incorrectArgs ())
    make "syscallFcntlGetPath" [ integer "fd" ] TBlob KTBlob
      (fun _ vm values ->
        match values with
        | [| DInt fd |] -> perform KTBlob vm (Op.PosixOp.FcntlGetPath(intToDescriptor vm fd)) bytes
        | _ -> incorrectArgs ())
    make "syscallSysctl"
      [ Param.make "mib" (TList TInt) "Read-only numeric key"; integer "count" ] TBlob KTBlob
      (fun _ vm values ->
        match values with
        | [| DList(_, mib); DInt count |] ->
          let mib = mib |> List.map (function DInt n -> intToInt32 vm n | _ -> incorrectArgs ())
          perform KTBlob vm (Op.PosixOp.SysctlBytes(mib, intToInt32 vm count)) bytes
        | _ -> incorrectArgs ())
    make "syscallSchedGetaffinity" [ integer "pid"; integer "count" ] TBlob KTBlob
      (fun _ vm values ->
        match values with
        | [| DInt pid; DInt count |] ->
          perform KTBlob vm
            (Op.PosixOp.SchedGetaffinity(intToInt32 vm pid, intToInt32 vm count)) bytes
        | _ -> incorrectArgs ())
    { (make "syscallGetentropy" [ integer "count" ] TBlob KTBlob
        (fun _ vm values ->
          match values with
          | [| DInt count |] ->
            perform KTBlob vm (Op.PosixOp.Getentropy(intToInt32 vm count)) bytes
          | _ -> incorrectArgs ()))
        with callEffects = set [ Effect.Random ] }
    make "syscallCopyFileRange"
      [ integer "source"; integer "destination"; integer "count" ] TInt KTInt
      (fun _ vm values ->
        match values with
        | [| DInt source; DInt destination; DInt count |] ->
          let op = Op.PosixOp.CopyFileRange(
            intToDescriptor vm source, intToDescriptor vm destination, intToInt32 vm count)
          perform KTInt vm op offset
        | _ -> incorrectArgs ())
    make "syscallCloneFile" [ integer "source"; integer "destination" ] TUnit KTUnit
      (fun _ vm values ->
        match values with
        | [| DInt source; DInt destination |] ->
          perform KTUnit vm
            (Op.PosixOp.CloneFile(intToDescriptor vm source, intToDescriptor vm destination)) unit
        | _ -> incorrectArgs ())
    make "syscallFtruncate" [ integer "fd"; integer "length" ] TUnit KTUnit
      (fun _ vm values ->
        match values with
        | [| DInt fd; DInt length |] ->
          perform KTUnit vm
            (Op.PosixOp.Ftruncate(intToDescriptor vm fd, intToInt64 vm length)) unit
        | _ -> incorrectArgs ())
    make "syscallFchmod" [ integer "fd"; integer "mode" ] TUnit KTUnit
      (fun _ vm values ->
        match values with
        | [| DInt fd; DInt mode |] ->
          perform KTUnit vm
            (Op.PosixOp.Fchmod(intToDescriptor vm fd, intToInt32 vm mode)) unit
        | _ -> incorrectArgs ())
    make "syscallFsync" [ integer "fd" ] TUnit KTUnit
      (fun _ vm values ->
        match values with
        | [| DInt fd |] -> perform KTUnit vm (Op.PosixOp.Fsync(intToDescriptor vm fd)) unit
        | _ -> incorrectArgs ())
    make "syscallFstat" [ integer "fd" ] TBlob KTBlob
      (fun _ vm values ->
        match values with
        | [| DInt fd |] -> perform KTBlob vm (Op.PosixOp.Fstat(intToDescriptor vm fd)) bytes
        | _ -> incorrectArgs ())
    make "syscallPread" [ integer "fd"; integer "count"; integer "offset" ] TBlob KTBlob
      (fun _ vm values ->
        match values with
        | [| DInt fd; DInt count; DInt offset |] ->
          perform KTBlob vm
            (Op.PosixOp.Pread(intToDescriptor vm fd, intToInt32 vm count,
              intToInt64 vm offset)) bytes
        | _ -> incorrectArgs ())
    make "syscallPwrite" [ integer "fd"; blob "bytes"; integer "offset" ] TInt KTInt
      (fun state vm values ->
        match values with
        | [| DInt fd; DBlob reference; DInt offset |] ->
          Blob.withBytes state reference (fun bytes ->
            perform KTInt vm
              (Op.PosixOp.Pwrite(intToDescriptor vm fd, bytes, intToInt64 vm offset)) written)
        | _ -> incorrectArgs ()) ]

let builtins () : Builtins = Builtin.make [] (fns ())
