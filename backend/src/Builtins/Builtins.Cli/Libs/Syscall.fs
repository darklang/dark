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

// Conditional file/Native authorization is derived from the descriptor
// by Host. No ambient Native gate may reject a scoped file capability.
// `Raise`: nearly all narrow a descriptor or an integer to the kernel's width.
let private make symbol arity body =
  Builtins.Cli.Libs.Posix.provider symbol arity (set [ Effect.Raise ]) body

let private bytes r = Bytes.newEphemeral (Host.expectBytes r)
let private unit (_ : Host.Response) = DUnit
let private offset r = Dval.dint64 (int64 (Host.expectOffset r))
let private written r = Dval.dint64 (int64 (Host.expectWritten r))

let private timestamp name constructor : BuiltInFn =
  { (make name 5 (fun _ vm values ->
      match values with
      | [| DString path; DInt64 a; DInt64 an; DInt64 m; DInt64 mn |] ->
        perform KTUnit vm (constructor (path, a, an, m, mn)) unit
      | _ -> incorrectArgs ())) with
      callEffects = set [ Effect.FileWrite ] }

let private clockUnit name op =
  { (make name 1 (fun _ vm values ->
      match values with
      | [| DUnit |] -> perform KTBytes vm op bytes
      | _ -> incorrectArgs ())) with
      callEffects = set [ Effect.Clock ] }

let private poll name constructor =
  make name 2 (fun _ vm values ->
    match values with
    | [| DList(_, Items(descriptors)); DInt64 timeout |] ->
      let descriptors =
        descriptors
        |> List.map (function
          | DTuple(DBigInt fd, DInt64 events, []) ->
            bigIntToDescriptor fd, int64ToInt32 events
          | _ -> incorrectArgs ())
      perform KTBytes vm (constructor (descriptors, int64ToInt32 timeout)) bytes
    | _ -> incorrectArgs ())

let fns () : List<BuiltInFn> =
  [ { (make "kqueue" 1 (fun _ vm values ->
        match values with
        | [| DUnit |] ->
          perform
            KTBigInt
            vm
            Op.PosixOp.Kqueue
            (Host.expectDescriptor >> Dval.bigInt)
        | _ -> incorrectArgs ())) with
        callEffects = Set.empty }
    make "keventVnode" 4 (fun _ vm values ->
      match values with
      | [| DBigInt queue; DList(_, Items(changes)); DInt64 capacity; DInt64 timeout |] ->
        let changes =
          changes
          |> List.map (function
            | DTuple(DBigInt fd, DInt64 flags, [ DInt64 mask ]) ->
              bigIntToDescriptor fd, int64ToInt32 flags, int64ToInt32 mask
            | _ -> incorrectArgs ())
        perform
          KTBytes
          vm
          (Op.PosixOp.KeventVnode(
            bigIntToDescriptor queue,
            changes,
            int64ToInt32 capacity,
            int64ToInt32 timeout
          ))
          bytes
      | _ -> incorrectArgs ())
    make "inotifyInit1" 1 (fun _ vm values ->
      match values with
      | [| DInt64 flags |] ->
        perform
          KTBigInt
          vm
          (Op.PosixOp.InotifyInit1(int64ToInt32 flags))
          (fun response -> Dval.bigInt (Host.expectDescriptor response))
      | _ -> incorrectArgs ())
    make "inotifyAddWatch" 3 (fun _ vm values ->
      match values with
      | [| DBigInt fd; DString path; DInt64 mask |] ->
        perform
          KTInt64
          vm
          (Op.PosixOp.InotifyAddWatch(
            bigIntToDescriptor fd,
            path,
            int64ToInt32 mask
          ))
          written
      | _ -> incorrectArgs ())
    poll "ppoll" Op.PosixOp.Ppoll
    poll "pollDarwin" Op.PosixOp.PollDarwin
    { (make "clockGettime" 1 (fun _ vm values ->
        match values with
        | [| DInt64 id |] ->
          perform KTBytes vm (Op.PosixOp.ClockGettime(int64ToInt32 id)) bytes
        | _ -> incorrectArgs ())) with
        callEffects = set [ Effect.Clock; Effect.Raise ] }
    clockUnit "gettimeofday" Op.PosixOp.Gettimeofday
    clockUnit "machAbsoluteTime" Op.PosixOp.MachAbsoluteTime
    clockUnit "machTimebaseInfo" Op.PosixOp.MachTimebaseInfo
    { (make "nanosleep" 2 (fun _ vm values ->
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
        | _ -> incorrectArgs ())) with
        callEffects = set [ Effect.Clock ] }
    timestamp "utimensAt" Op.PosixOp.UtimensAt
    timestamp "setattrlistTimes" Op.PosixOp.SetattrlistTimes
    { (make "uname" 1 (fun _ vm values ->
        match values with
        | [| DUnit |] -> perform KTBytes vm Op.PosixOp.UnameBytes bytes
        | _ -> incorrectArgs ())) with
        callEffects = Set.empty }
    make "fcntlGetPath" 1 (fun _ vm values ->
      match values with
      | [| DBigInt fd |] ->
        perform KTBytes vm (Op.PosixOp.FcntlGetPath(bigIntToDescriptor fd)) bytes
      | _ -> incorrectArgs ())
    make "sysctl" 2 (fun _ vm values ->
      match values with
      | [| DList(_, Items(mib)); DInt64 count |] ->
        let mib =
          mib
          |> List.map (function
            | DInt64 n -> int64ToInt32 n
            | _ -> incorrectArgs ())
        perform KTBytes vm (Op.PosixOp.SysctlBytes(mib, int64ToInt32 count)) bytes
      | _ -> incorrectArgs ())
    make "schedGetaffinity" 2 (fun _ vm values ->
      match values with
      | [| DInt64 pid; DInt64 count |] ->
        perform
          KTBytes
          vm
          (Op.PosixOp.SchedGetaffinity(int64ToInt32 pid, int64ToInt32 count))
          bytes
      | _ -> incorrectArgs ())
    { (make "getentropy" 1 (fun _ vm values ->
        match values with
        | [| DInt64 count |] ->
          perform KTBytes vm (Op.PosixOp.Getentropy(int64ToInt32 count)) bytes
        | _ -> incorrectArgs ())) with
        callEffects = set [ Effect.Random; Effect.Raise ] }
    make "copyFileRange" 3 (fun _ vm values ->
      match values with
      | [| DBigInt source; DBigInt destination; DInt64 count |] ->
        let op =
          Op.PosixOp.CopyFileRange(
            bigIntToDescriptor source,
            bigIntToDescriptor destination,
            int64ToInt32 count
          )
        perform KTInt64 vm op offset
      | _ -> incorrectArgs ())
    make "cloneFile" 2 (fun _ vm values ->
      match values with
      | [| DBigInt source; DBigInt destination |] ->
        let op =
          Op.PosixOp.CloneFile(
            bigIntToDescriptor source,
            bigIntToDescriptor destination
          )
        perform KTUnit vm op unit
      | _ -> incorrectArgs ())
    make "fclonefileAt" 3 (fun _ vm values ->
      match values with
      | [| DBigInt source; DString destination; DInt64 flags |] ->
        let op =
          Op.PosixOp.FclonefileAt(
            bigIntToDescriptor source,
            destination,
            int64ToInt32 flags
          )
        perform KTUnit vm op unit
      | _ -> incorrectArgs ())
    make "ftruncate" 2 (fun _ vm values ->
      match values with
      | [| DBigInt fd; DInt64 length |] ->
        perform KTUnit vm (Op.PosixOp.Ftruncate(bigIntToDescriptor fd, length)) unit
      | _ -> incorrectArgs ())
    make "fchmod" 2 (fun _ vm values ->
      match values with
      | [| DBigInt fd; DInt64 mode |] ->
        let op = Op.PosixOp.Fchmod(bigIntToDescriptor fd, int64ToInt32 mode)
        perform KTUnit vm op unit
      | _ -> incorrectArgs ())
    make "fstat" 1 (fun _ vm values ->
      match values with
      | [| DBigInt fd |] ->
        perform KTBytes vm (Op.PosixOp.Fstat(bigIntToDescriptor fd)) bytes
      | _ -> incorrectArgs ())
    make "pread" 3 (fun _ vm values ->
      match values with
      | [| DBigInt fd; DInt64 count; DInt64 offset |] ->
        let op = Op.PosixOp.Pread(bigIntToDescriptor fd, int64ToInt32 count, offset)
        perform KTBytes vm op bytes
      | _ -> incorrectArgs ())
    make "pwrite" 3 (fun state vm values ->
      match values with
      | [| DBigInt fd; DBytes reference; DInt64 offset |] ->
        Bytes.withBytes state reference (fun bytes ->
          perform
            KTInt64
            vm
            (Op.PosixOp.Pwrite(bigIntToDescriptor fd, bytes, offset))
            written)
      | _ -> incorrectArgs ()) ]

let builtins () : Builtins = Builtin.make [] (fns ())
