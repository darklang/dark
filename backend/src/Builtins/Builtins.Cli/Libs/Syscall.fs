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

let private clock name parameters body =
  { (make name parameters TBlob KTBlob body)
      with callEffects = set [ Effect.Clock ] }

let private clockUnit name op =
  clock name [ Param.make "unit" TUnit "" ] (fun _ vm values ->
    match values with
    | [| DUnit |] -> perform KTBlob vm op bytes
    | _ -> incorrectArgs ())

let private poll name constructor =
  make name
    [ Param.make "descriptors" (TList(TTuple(TInt, TInt, []))) "fd and requested events"
      integer "timeoutMs" ] TBlob KTBlob
    (fun _ vm values ->
      match values with
      | [| DList(_, descriptors); DInt timeout |] ->
        let descriptors = descriptors |> List.map (function
          | DTuple(DInt fd, DInt events, []) -> intToDescriptor vm fd, intToInt32 vm events
          | _ -> incorrectArgs ())
        perform KTBlob vm (constructor(descriptors, intToInt32 vm timeout)) bytes
      | _ -> incorrectArgs ())

let fns () : List<BuiltInFn> =
  [ make "syscallKqueue" [ Param.make "unit" TUnit "" ] TInt KTInt
      (fun _ vm values ->
        match values with
        | [| DUnit |] -> perform KTInt vm Op.PosixOp.Kqueue (Host.expectDescriptor >> Dval.int)
        | _ -> incorrectArgs ())
    make "syscallKeventVnode"
      [ integer "queue"; Param.make "changes" (TList(TTuple(TInt, TInt, [ TInt ]))) "fd, flags, vnode mask"
        integer "capacity"; integer "timeoutMs" ] TBlob KTBlob
      (fun _ vm values ->
        match values with
        | [| DInt queue; DList(_, changes); DInt capacity; DInt timeout |] ->
          let changes = changes |> List.map (function
            | DTuple(DInt fd, DInt flags, [ DInt mask ]) ->
              intToDescriptor vm fd, intToInt32 vm flags, intToInt32 vm mask
            | _ -> incorrectArgs ())
          perform KTBlob vm (Op.PosixOp.KeventVnode(intToDescriptor vm queue, changes,
            intToInt32 vm capacity, intToInt32 vm timeout)) bytes
        | _ -> incorrectArgs ())
    make "syscallInotifyInit1" [ integer "flags" ] TInt KTInt
      (fun _ vm values ->
        match values with
        | [| DInt flags |] ->
          perform KTInt vm (Op.PosixOp.InotifyInit1(intToInt32 vm flags))
            (fun response -> Dval.int (Host.expectDescriptor response))
        | _ -> incorrectArgs ())
    make "syscallInotifyAddWatch"
      [ integer "fd"; Param.make "path" TString "Checked path"; integer "mask" ] TInt KTInt
      (fun _ vm values ->
        match values with
        | [| DInt fd; DString path; DInt mask |] ->
          perform KTInt vm (Op.PosixOp.InotifyAddWatch(intToDescriptor vm fd, path, intToInt32 vm mask)) written
        | _ -> incorrectArgs ())
    poll "syscallPpoll" Op.PosixOp.Ppoll
    poll "syscallPollDarwin" Op.PosixOp.PollDarwin
    clock "syscallClockGettime" [ integer "clock" ] (fun _ vm values ->
      match values with
      | [| DInt id |] -> perform KTBlob vm (Op.PosixOp.ClockGettime(intToInt32 vm id)) bytes
      | _ -> incorrectArgs ())
    clockUnit "syscallGettimeofday" Op.PosixOp.Gettimeofday
    clockUnit "syscallMachAbsoluteTime" Op.PosixOp.MachAbsoluteTime
    clockUnit "syscallMachTimebaseInfo" Op.PosixOp.MachTimebaseInfo
    { name = fn "syscallNanosleep" 0
      typeParams = []
      parameters = [ integer "seconds"; integer "nanos" ]
      returnType = TTuple(TInt, TBlob, [])
      description = "One nanosleep attempt: raw status and remaining timespec"
      fn = (fun struct (_, vm, _, values) ->
        match values with
        | [| DInt seconds; DInt nanos |] ->
          Interpreter.requestHost vm
            (Host.Operation.Posix(Op.PosixOp.Nanosleep(intToInt64 vm seconds, intToInt64 vm nanos)))
            (fun outcome ->
              match outcome with
              | Ok(Op.Response.KernelSleep(rc, remaining)) ->
                Ply(DTuple(Dval.int (bigint rc), Blob.newEphemeral remaining, []))
              | _ -> incorrectArgs ())
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Clock ]
      deprecated = NotDeprecated }
    timestamp "syscallUtimensAt" Op.PosixOp.UtimensAt
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
    make "syscallFclonefileAt" [ integer "source"; Param.make "destination" TString "Checked destination path"; integer "flags" ] TUnit KTUnit
      (fun _ vm values ->
        match values with
        | [| DInt source; DString destination; DInt flags |] ->
          perform KTUnit vm (Op.PosixOp.FclonefileAt(intToDescriptor vm source, destination, intToInt32 vm flags)) unit
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
