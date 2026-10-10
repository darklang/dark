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
    callEffects = set [ Effect.Native ]
    deprecated = NotDeprecated }

let private integer name = Param.make name TInt "Kernel scalar"
let private blob name = Param.make name TBlob "Bytes"
let private bytes r = Blob.newEphemeral (Host.expectBytes r)
let private unit (_ : Host.Response) = DUnit
let private offset r = Dval.int (bigint (Host.expectOffset r))
let private written r = Dval.int (bigint (Host.expectWritten r))

let fns () : List<BuiltInFn> =
  [ { (make "syscallGetentropy" [ integer "count" ] TBlob KTBlob
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
            intToInt32 vm source, intToInt32 vm destination, intToInt32 vm count)
          perform KTInt vm op offset
        | _ -> incorrectArgs ())
    make "syscallCloneFile" [ integer "source"; integer "destination" ] TUnit KTUnit
      (fun _ vm values ->
        match values with
        | [| DInt source; DInt destination |] ->
          perform KTUnit vm
            (Op.PosixOp.CloneFile(intToInt32 vm source, intToInt32 vm destination)) unit
        | _ -> incorrectArgs ())
    make "syscallFtruncate" [ integer "fd"; integer "length" ] TUnit KTUnit
      (fun _ vm values ->
        match values with
        | [| DInt fd; DInt length |] ->
          perform KTUnit vm
            (Op.PosixOp.Ftruncate(intToInt32 vm fd, intToInt64 vm length)) unit
        | _ -> incorrectArgs ())
    make "syscallFchmod" [ integer "fd"; integer "mode" ] TUnit KTUnit
      (fun _ vm values ->
        match values with
        | [| DInt fd; DInt mode |] ->
          perform KTUnit vm
            (Op.PosixOp.Fchmod(intToInt32 vm fd, intToInt32 vm mode)) unit
        | _ -> incorrectArgs ())
    make "syscallFsync" [ integer "fd" ] TUnit KTUnit
      (fun _ vm values ->
        match values with
        | [| DInt fd |] -> perform KTUnit vm (Op.PosixOp.Fsync(intToInt32 vm fd)) unit
        | _ -> incorrectArgs ())
    make "syscallFstat" [ integer "fd" ] TBlob KTBlob
      (fun _ vm values ->
        match values with
        | [| DInt fd |] -> perform KTBlob vm (Op.PosixOp.Fstat(intToInt32 vm fd)) bytes
        | _ -> incorrectArgs ())
    make "syscallPread" [ integer "fd"; integer "count"; integer "offset" ] TBlob KTBlob
      (fun _ vm values ->
        match values with
        | [| DInt fd; DInt count; DInt offset |] ->
          perform KTBlob vm
            (Op.PosixOp.Pread(intToInt32 vm fd, intToInt32 vm count,
              intToInt64 vm offset)) bytes
        | _ -> incorrectArgs ())
    make "syscallPwrite" [ integer "fd"; blob "bytes"; integer "offset" ] TInt KTInt
      (fun state vm values ->
        match values with
        | [| DInt fd; DBlob reference; DInt offset |] ->
          Blob.withBytes state reference (fun bytes ->
            perform KTInt vm
              (Op.PosixOp.Pwrite(intToInt32 vm fd, bytes, intToInt64 vm offset)) written)
        | _ -> incorrectArgs ()) ]

let builtins () : Builtins = Builtin.make [] (fns ())
