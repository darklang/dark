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
    | Ok response -> (Dval.resultOk kt KTInt64 (shape response))
    | Error e -> (Dval.resultError kt KTInt64 (Dval.dint64 (int64 e.errno))))

/// An argument the kernel's `int` cannot hold, answered as the kernel would: EBADF for a
/// descriptor, EINVAL for anything else.
let private kernelRejects kt (args : array<Dval>) : Dval =
  let badFd =
    args
    |> Array.exists (function
      | DBigInt(KernelFd _) -> false
      | DBigInt _ -> true
      | _ -> false)
  Dval.resultError kt KTInt64 (Dval.dint64 (if badFd then 9L else 22L))

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
    callEffects = Set.empty
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
        let narrowed =
          descriptors
          |> List.choose (function
            | DTuple(DBigInt(KernelFd fd), DInt64(KernelInt events), []) ->
              Some(fd, events)
            | DTuple(DBigInt _, DInt64 _, []) -> None
            | _ -> incorrectArgs ())
        match timeout with
        | KernelInt timeout when narrowed.Length = descriptors.Length ->
          perform KTBytes vm (constructor (narrowed, timeout)) bytes
        | _ -> kernelRejects KTBytes values
      | _ -> incorrectArgs ())

let fns () : List<BuiltInFn> =
  [ make
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
        | _ -> incorrectArgs ())
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
          let narrowed =
            changes
            |> List.choose (function
              | DTuple(DBigInt(KernelFd fd),
                       DInt64(KernelInt flags),
                       [ DInt64(KernelInt mask) ]) -> Some(fd, flags, mask)
              | DTuple(DBigInt _, DInt64 _, [ DInt64 _ ]) -> None
              | _ -> incorrectArgs ())
          match queue, capacity, timeout with
          | KernelFd queue, KernelInt capacity, KernelInt timeout when
            narrowed.Length = changes.Length
            ->
            perform
              KTBytes
              vm
              (Op.PosixOp.KeventVnode(queue, narrowed, capacity, timeout))
              bytes
          | _ -> kernelRejects KTBytes values
        | _ -> incorrectArgs ())
    make
      "syscallInotifyInit1"
      [ integer "flags" ]
      TBigInt
      KTBigInt
      (fun _ vm values ->
        match values with
        | [| DInt64(KernelInt flags) |] ->
          perform KTBigInt vm (Op.PosixOp.InotifyInit1 flags) (fun response ->
            Dval.bigInt (Host.expectDescriptor response))
        | [| DInt64 _ |] -> kernelRejects KTBigInt values
        | _ -> incorrectArgs ())
    make
      "syscallInotifyAddWatch"
      [ descriptor "fd"; Param.make "path" TString "Checked path"; integer "mask" ]
      TInt64
      KTInt64
      (fun _ vm values ->
        match values with
        | [| DBigInt(KernelFd fd); DString path; DInt64(KernelInt mask) |] ->
          perform KTInt64 vm (Op.PosixOp.InotifyAddWatch(fd, path, mask)) written
        | [| DBigInt _; DString _; DInt64 _ |] -> kernelRejects KTInt64 values
        | _ -> incorrectArgs ())
    poll "syscallPpoll" Op.PosixOp.Ppoll
    poll "syscallPollDarwin" Op.PosixOp.PollDarwin
    { (clock "syscallClockGettime" [ integer "clock" ] (fun _ vm values ->
        match values with
        | [| DInt64(KernelInt id) |] ->
          perform KTBytes vm (Op.PosixOp.ClockGettime id) bytes
        | [| DInt64 _ |] -> kernelRejects KTBytes values
        | _ -> incorrectArgs ())) with
        callEffects = set [ Effect.Clock ] }
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
                  (DTuple(Dval.dint64 (int64 rc), Bytes.newEphemeral remaining, []))
                | _ -> incorrectArgs ())
          | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.Clock ]
      deprecated = NotDeprecated }
    timestamp "syscallUtimensAt" Op.PosixOp.UtimensAt
    timestamp "syscallSetattrlistTimes" Op.PosixOp.SetattrlistTimes
    make
      "syscallUname"
      [ Param.make "unit" TUnit "" ]
      TBytes
      KTBytes
      (fun _ vm values ->
        match values with
        | [| DUnit |] -> perform KTBytes vm Op.PosixOp.UnameBytes bytes
        | _ -> incorrectArgs ())
    make "syscallFcntlGetPath" [ descriptor "fd" ] TBytes KTBytes (fun _ vm values ->
      match values with
      | [| DBigInt(KernelFd fd) |] ->
        perform KTBytes vm (Op.PosixOp.FcntlGetPath fd) bytes
      | [| DBigInt _ |] -> kernelRejects KTBytes values
      | _ -> incorrectArgs ())
    make
      "syscallSysctl"
      [ Param.make "mib" (TList TInt64) "Read-only numeric key"; integer "count" ]
      TBytes
      KTBytes
      (fun _ vm values ->
        match values with
        | [| DList(_, Items(mib)); DInt64 count |] ->
          let narrowed =
            mib
            |> List.choose (function
              | DInt64(KernelInt n) -> Some n
              | DInt64 _ -> None
              | _ -> incorrectArgs ())
          match count with
          | KernelInt count when narrowed.Length = mib.Length ->
            perform KTBytes vm (Op.PosixOp.SysctlBytes(narrowed, count)) bytes
          | _ -> kernelRejects KTBytes values
        | _ -> incorrectArgs ())
    make
      "syscallSchedGetaffinity"
      [ integer "pid"; integer "count" ]
      TBytes
      KTBytes
      (fun _ vm values ->
        match values with
        | [| DInt64(KernelInt pid); DInt64(KernelInt count) |] ->
          perform KTBytes vm (Op.PosixOp.SchedGetaffinity(pid, count)) bytes
        | [| DInt64 _; DInt64 _ |] -> kernelRejects KTBytes values
        | _ -> incorrectArgs ())
    { (make
        "syscallGetentropy"
        [ integer "count" ]
        TBytes
        KTBytes
        (fun _ vm values ->
          match values with
          | [| DInt64(KernelInt count) |] ->
            perform KTBytes vm (Op.PosixOp.Getentropy count) bytes
          | [| DInt64 _ |] -> kernelRejects KTBytes values
          | _ -> incorrectArgs ())) with
        callEffects = set [ Effect.Random ] }
    make
      "syscallCopyFileRange"
      [ descriptor "source"; descriptor "destination"; integer "count" ]
      TInt64
      KTInt64
      (fun _ vm values ->
        match values with
        | [| DBigInt(KernelFd source)
             DBigInt(KernelFd destination)
             DInt64(KernelInt count) |] ->
          perform
            KTInt64
            vm
            (Op.PosixOp.CopyFileRange(source, destination, count))
            offset
        | [| DBigInt _; DBigInt _; DInt64 _ |] -> kernelRejects KTInt64 values
        | _ -> incorrectArgs ())
    make
      "syscallCloneFile"
      [ descriptor "source"; descriptor "destination" ]
      TUnit
      KTUnit
      (fun _ vm values ->
        match values with
        | [| DBigInt(KernelFd source); DBigInt(KernelFd destination) |] ->
          perform KTUnit vm (Op.PosixOp.CloneFile(source, destination)) unit
        | [| DBigInt _; DBigInt _ |] -> kernelRejects KTUnit values
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
        | [| DBigInt(KernelFd source); DString destination; DInt64(KernelInt flags) |] ->
          perform
            KTUnit
            vm
            (Op.PosixOp.FclonefileAt(source, destination, flags))
            unit
        | [| DBigInt _; DString _; DInt64 _ |] -> kernelRejects KTUnit values
        | _ -> incorrectArgs ())
    make
      "syscallFtruncate"
      [ descriptor "fd"; integer "length" ]
      TUnit
      KTUnit
      (fun _ vm values ->
        match values with
        | [| DBigInt(KernelFd fd); DInt64 length |] ->
          perform KTUnit vm (Op.PosixOp.Ftruncate(fd, length)) unit
        | [| DBigInt _; DInt64 _ |] -> kernelRejects KTUnit values
        | _ -> incorrectArgs ())
    make
      "syscallFchmod"
      [ descriptor "fd"; integer "mode" ]
      TUnit
      KTUnit
      (fun _ vm values ->
        match values with
        | [| DBigInt(KernelFd fd); DInt64(KernelInt mode) |] ->
          perform KTUnit vm (Op.PosixOp.Fchmod(fd, mode)) unit
        | [| DBigInt _; DInt64 _ |] -> kernelRejects KTUnit values
        | _ -> incorrectArgs ())
    make "syscallFstat" [ descriptor "fd" ] TBytes KTBytes (fun _ vm values ->
      match values with
      | [| DBigInt(KernelFd fd) |] -> perform KTBytes vm (Op.PosixOp.Fstat fd) bytes
      | [| DBigInt _ |] -> kernelRejects KTBytes values
      | _ -> incorrectArgs ())
    make
      "syscallPread"
      [ descriptor "fd"; integer "count"; integer "offset" ]
      TBytes
      KTBytes
      (fun _ vm values ->
        match values with
        | [| DBigInt(KernelFd fd); DInt64(KernelInt count); DInt64 offset |] ->
          perform KTBytes vm (Op.PosixOp.Pread(fd, count, offset)) bytes
        | [| DBigInt _; DInt64 _; DInt64 _ |] -> kernelRejects KTBytes values
        | _ -> incorrectArgs ())
    make
      "syscallPwrite"
      [ descriptor "fd"; bytesParam "bytes"; integer "offset" ]
      TInt64
      KTInt64
      (fun state vm values ->
        match values with
        | [| DBigInt(KernelFd fd); DBytes reference; DInt64 offset |] ->
          Bytes.withBytes state reference (fun bytes ->
            perform KTInt64 vm (Op.PosixOp.Pwrite(fd, bytes, offset)) written)
        | [| DBigInt _; DBytes _; DInt64 _ |] -> kernelRejects KTInt64 values
        | _ -> incorrectArgs ()) ]

let builtins () : Builtins = Builtin.make [] (fns ())
