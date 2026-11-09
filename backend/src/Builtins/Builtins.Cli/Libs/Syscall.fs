/// Single kernel operations. Dark libraries own retries and all algorithms.
module Builtins.Cli.Libs.Syscall

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module Bytes = LibExecution.Bytes
module Host = LibExecution.Host
module Op = LibExecution.HostTypes
module Interpreter = LibExecution.Interpreter
module Builtin = LibExecution.Builtin

let private perform kt vm op shape =
  Interpreter.requestHost vm (Host.Operation.Posix op) (fun result ->
    match result with
    | Ok response -> Ply(Dval.resultOk kt KTInt64 (shape response))
    | Error e -> Ply(Dval.resultError kt KTInt64 (Dval.dint64 (int64 e.errno))))

let private make name parameters result _kt body : BuiltInFn =
  { name = fn name 0
    typeParams = []
    parameters = parameters
    returnType = TypeReference.result result TInt64
    description = "One kernel operation; numeric errno, no retries or decoding"
    fn = (fun struct (state, vm, _, values) -> body state vm values)
    previewable = Impure
    // Conditional file/Native authorization is derived from the descriptor
    // by Host. No ambient Native gate may reject a scoped file capability.
    // `Raise`: nearly all narrow a descriptor or an integer to the kernel's width.
    callEffects = set [ Effect.Raise ]
    deprecated = NotDeprecated }

let private integer name = Param.make name TInt64 "Kernel scalar"
let private descriptor name = Param.make name TBigInt "Descriptor token"
let private bytesParam name = Param.make name TBytes "Bytes"
let private bytes r = Bytes.newEphemeral (Host.expectBytes r)
let private unit (_ : Host.Response) = DUnit
let private offset r = Dval.dint64 (int64 (Host.expectOffset r))
let private written r = Dval.dint64 (int64 (Host.expectWritten r))

let private timestamp name constructor : BuiltInFn =
  { (make
      name
      [ Param.make "path" TString "Checked path"
        integer "accessSeconds"
        integer "accessNanos"
        integer "modifySeconds"
        integer "modifyNanos" ]
      TUnit
      KTUnit
      (fun _ vm values ->
        match values with
        | [| DString path; DInt64 a; DInt64 an; DInt64 m; DInt64 mn |] ->
          perform KTUnit vm (constructor (path, a, an, m, mn)) unit
        | _ -> incorrectArgs ())) with
      callEffects = set [ Effect.FileWrite ] }

let private clock name parameters body =
  { (make name parameters TBytes KTBytes body) with
      callEffects = set [ Effect.Clock ] }

let private clockUnit name op =
  clock name [ Param.make "unit" TUnit "" ] (fun _ vm values ->
    match values with
    | [| DUnit |] -> perform KTBytes vm op bytes
    | _ -> incorrectArgs ())

let private poll name constructor =
  make
    name
    [ Param.make
        "descriptors"
        (TList(TTuple(TBigInt, TInt64, [])))
        "fd and requested events"
      integer "timeoutMs" ]
    TBytes
    KTBytes
    (fun _ vm values ->
      match values with
      | [| DList(_, Items(descriptors)); DInt64 timeout |] ->
        let descriptors =
          descriptors
          |> List.map (function
            | DTuple(DBigInt fd, DInt64 events, []) ->
              bigIntToDescriptor vm fd, int64ToInt32 vm events
            | _ -> incorrectArgs ())
        perform
          KTBytes
          vm
          (constructor (descriptors, int64ToInt32 vm timeout))
          bytes
      | _ -> incorrectArgs ())

let fns () : List<BuiltInFn> =
  [ { (make
        "syscallKqueue"
        [ Param.make "unit" TUnit "" ]
        TBigInt
        KTBigInt
        (fun _ vm values ->
          match values with
          | [| DUnit |] ->
            perform
              KTBigInt
              vm
              Op.PosixOp.Kqueue
              (Host.expectDescriptor >> Dval.bigInt)
          | _ -> incorrectArgs ())) with
        callEffects = Set.empty }
    make
      "syscallKeventVnode"
      [ descriptor "queue"
        Param.make
          "changes"
          (TList(TTuple(TBigInt, TInt64, [ TInt64 ])))
          "fd, flags, vnode mask"
        integer "capacity"
        integer "timeoutMs" ]
      TBytes
      KTBytes
      (fun _ vm values ->
        match values with
        | [| DBigInt queue
             DList(_, Items(changes))
             DInt64 capacity
             DInt64 timeout |] ->
          let changes =
            changes
            |> List.map (function
              | DTuple(DBigInt fd, DInt64 flags, [ DInt64 mask ]) ->
                bigIntToDescriptor vm fd,
                int64ToInt32 vm flags,
                int64ToInt32 vm mask
              | _ -> incorrectArgs ())
          perform
            KTBytes
            vm
            (Op.PosixOp.KeventVnode(
              bigIntToDescriptor vm queue,
              changes,
              int64ToInt32 vm capacity,
              int64ToInt32 vm timeout
            ))
            bytes
        | _ -> incorrectArgs ())
    make
      "syscallInotifyInit1"
      [ integer "flags" ]
      TBigInt
      KTBigInt
      (fun _ vm values ->
        match values with
        | [| DInt64 flags |] ->
          perform
            KTBigInt
            vm
            (Op.PosixOp.InotifyInit1(int64ToInt32 vm flags))
            (fun response -> Dval.bigInt (Host.expectDescriptor response))
        | _ -> incorrectArgs ())
    make
      "syscallInotifyAddWatch"
      [ descriptor "fd"; Param.make "path" TString "Checked path"; integer "mask" ]
      TInt64
      KTInt64
      (fun _ vm values ->
        match values with
        | [| DBigInt fd; DString path; DInt64 mask |] ->
          perform
            KTInt64
            vm
            (Op.PosixOp.InotifyAddWatch(
              bigIntToDescriptor vm fd,
              path,
              int64ToInt32 vm mask
            ))
            written
        | _ -> incorrectArgs ())
    poll "syscallPpoll" Op.PosixOp.Ppoll
    poll "syscallPollDarwin" Op.PosixOp.PollDarwin
    { (clock "syscallClockGettime" [ integer "clock" ] (fun _ vm values ->
        match values with
        | [| DInt64 id |] ->
          perform KTBytes vm (Op.PosixOp.ClockGettime(int64ToInt32 vm id)) bytes
        | _ -> incorrectArgs ())) with
        callEffects = set [ Effect.Clock; Effect.Raise ] }
    clockUnit "syscallGettimeofday" Op.PosixOp.Gettimeofday
    clockUnit "syscallMachAbsoluteTime" Op.PosixOp.MachAbsoluteTime
    clockUnit "syscallMachTimebaseInfo" Op.PosixOp.MachTimebaseInfo
    { name = fn "syscallNanosleep" 0
      typeParams = []
      parameters = [ integer "seconds"; integer "nanos" ]
      returnType = TTuple(TInt64, TBytes, [])
      description = "One nanosleep attempt: raw status and remaining timespec"
      fn =
        (fun struct (_, vm, _, values) ->
          match values with
          | [| DInt64 seconds; DInt64 nanos |] ->
            Interpreter.requestHost
              vm
              (Host.Operation.Posix(Op.PosixOp.Nanosleep(seconds, nanos)))
              (fun outcome ->
                match outcome with
                | Ok(Op.Response.KernelSleep(rc, remaining)) ->
                  Ply(
                    DTuple(
                      Dval.dint64 (int64 rc),
                      Bytes.newEphemeral remaining,
                      []
                    )
                  )
                | _ -> incorrectArgs ())
          | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.Clock ]
      deprecated = NotDeprecated }
    timestamp "syscallUtimensAt" Op.PosixOp.UtimensAt
    timestamp "syscallSetattrlistTimes" Op.PosixOp.SetattrlistTimes
    { (make
        "syscallUname"
        [ Param.make "unit" TUnit "" ]
        TBytes
        KTBytes
        (fun _ vm values ->
          match values with
          | [| DUnit |] -> perform KTBytes vm Op.PosixOp.UnameBytes bytes
          | _ -> incorrectArgs ())) with
        callEffects = Set.empty }
    make "syscallFcntlGetPath" [ descriptor "fd" ] TBytes KTBytes (fun _ vm values ->
      match values with
      | [| DBigInt fd |] ->
        perform KTBytes vm (Op.PosixOp.FcntlGetPath(bigIntToDescriptor vm fd)) bytes
      | _ -> incorrectArgs ())
    make
      "syscallSysctl"
      [ Param.make "mib" (TList TInt64) "Read-only numeric key"; integer "count" ]
      TBytes
      KTBytes
      (fun _ vm values ->
        match values with
        | [| DList(_, Items(mib)); DInt64 count |] ->
          let mib =
            mib
            |> List.map (function
              | DInt64 n -> int64ToInt32 vm n
              | _ -> incorrectArgs ())
          perform
            KTBytes
            vm
            (Op.PosixOp.SysctlBytes(mib, int64ToInt32 vm count))
            bytes
        | _ -> incorrectArgs ())
    make
      "syscallSchedGetaffinity"
      [ integer "pid"; integer "count" ]
      TBytes
      KTBytes
      (fun _ vm values ->
        match values with
        | [| DInt64 pid; DInt64 count |] ->
          perform
            KTBytes
            vm
            (Op.PosixOp.SchedGetaffinity(int64ToInt32 vm pid, int64ToInt32 vm count))
            bytes
        | _ -> incorrectArgs ())
    { (make
        "syscallGetentropy"
        [ integer "count" ]
        TBytes
        KTBytes
        (fun _ vm values ->
          match values with
          | [| DInt64 count |] ->
            perform KTBytes vm (Op.PosixOp.Getentropy(int64ToInt32 vm count)) bytes
          | _ -> incorrectArgs ())) with
        callEffects = set [ Effect.Random; Effect.Raise ] }
    make
      "syscallCopyFileRange"
      [ descriptor "source"; descriptor "destination"; integer "count" ]
      TInt64
      KTInt64
      (fun _ vm values ->
        match values with
        | [| DBigInt source; DBigInt destination; DInt64 count |] ->
          let op =
            Op.PosixOp.CopyFileRange(
              bigIntToDescriptor vm source,
              bigIntToDescriptor vm destination,
              int64ToInt32 vm count
            )
          perform KTInt64 vm op offset
        | _ -> incorrectArgs ())
    make
      "syscallCloneFile"
      [ descriptor "source"; descriptor "destination" ]
      TUnit
      KTUnit
      (fun _ vm values ->
        match values with
        | [| DBigInt source; DBigInt destination |] ->
          perform
            KTUnit
            vm
            (Op.PosixOp.CloneFile(
              bigIntToDescriptor vm source,
              bigIntToDescriptor vm destination
            ))
            unit
        | _ -> incorrectArgs ())
    make
      "syscallFclonefileAt"
      [ descriptor "source"
        Param.make "destination" TString "Checked destination path"
        integer "flags" ]
      TUnit
      KTUnit
      (fun _ vm values ->
        match values with
        | [| DBigInt source; DString destination; DInt64 flags |] ->
          perform
            KTUnit
            vm
            (Op.PosixOp.FclonefileAt(
              bigIntToDescriptor vm source,
              destination,
              int64ToInt32 vm flags
            ))
            unit
        | _ -> incorrectArgs ())
    make
      "syscallFtruncate"
      [ descriptor "fd"; integer "length" ]
      TUnit
      KTUnit
      (fun _ vm values ->
        match values with
        | [| DBigInt fd; DInt64 length |] ->
          perform
            KTUnit
            vm
            (Op.PosixOp.Ftruncate(bigIntToDescriptor vm fd, length))
            unit
        | _ -> incorrectArgs ())
    make
      "syscallFchmod"
      [ descriptor "fd"; integer "mode" ]
      TUnit
      KTUnit
      (fun _ vm values ->
        match values with
        | [| DBigInt fd; DInt64 mode |] ->
          perform
            KTUnit
            vm
            (Op.PosixOp.Fchmod(bigIntToDescriptor vm fd, int64ToInt32 vm mode))
            unit
        | _ -> incorrectArgs ())
    make "syscallFstat" [ descriptor "fd" ] TBytes KTBytes (fun _ vm values ->
      match values with
      | [| DBigInt fd |] ->
        perform KTBytes vm (Op.PosixOp.Fstat(bigIntToDescriptor vm fd)) bytes
      | _ -> incorrectArgs ())
    make
      "syscallPread"
      [ descriptor "fd"; integer "count"; integer "offset" ]
      TBytes
      KTBytes
      (fun _ vm values ->
        match values with
        | [| DBigInt fd; DInt64 count; DInt64 offset |] ->
          perform
            KTBytes
            vm
            (Op.PosixOp.Pread(
              bigIntToDescriptor vm fd,
              int64ToInt32 vm count,
              offset
            ))
            bytes
        | _ -> incorrectArgs ())
    make
      "syscallPwrite"
      [ descriptor "fd"; bytesParam "bytes"; integer "offset" ]
      TInt64
      KTInt64
      (fun state vm values ->
        match values with
        | [| DBigInt fd; DBytes reference; DInt64 offset |] ->
          Bytes.withBytes state reference (fun bytes ->
            perform
              KTInt64
              vm
              (Op.PosixOp.Pwrite(bigIntToDescriptor vm fd, bytes, offset))
              written)
        | _ -> incorrectArgs ()) ]

let builtins () : Builtins = Builtin.make [] (fns ())
