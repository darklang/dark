module LibExecution.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = RuntimeTypes
module RTE = RT.RuntimeError
module RT2DT = RuntimeTypesToDarkTypes
module Dval = LibExecution.Dval

let noTracing : RT.Tracing.Tracing =
  { loadFnResult = fun _ _ -> None
    storeFnResult = fun _ _ _ -> ()
    storeFrameEntry = fun _ _ _ -> ()
    storeLambdaResult = fun _ _ -> ()
    skipTracing = true }

let noTestContext : RT.TestContext =
  { sideEffectCount = 0

    exceptionReports = []
    expectedExceptionCount = 0
    postTestExecutionHook = fun _ -> () }

let createState
  (builtins : RT.Builtins)
  (pm : RT.PackageManager)
  (tracing : RT.Tracing.Tracing)
  (reportException : RT.ExceptionReporter)
  (notify : RT.Notifier)
  (branchId : RT.BranchId)
  (program : RT.Program)
  : RT.ExecutionState =
  { tracing = tracing
    test = noTestContext
    reportException = reportException
    notify = notify

    lambdaInstrCache = System.Collections.Concurrent.ConcurrentDictionary()
    packageFnInstrCache = System.Collections.Concurrent.ConcurrentDictionary()

    branchId = branchId
    program = program

    builtins = builtins
    types = { package = pm.getType }
    values = { builtIn = builtins.values; package = pm.getValue }
    blobs = { get = pm.getBlob; persist = pm.persistBlob }
    fns =
      { builtIn = builtins.fns
        package = pm.getFn
        isHarmful = fun pkg -> pm.isHarmful branchId pkg }

    allowHarmful = false

    // The base default is permissive — `createState` is RT-level and can't read the on-disk grant, so
    // the gate is a no-op here. The CLI host narrows it per entry point (`eval`/host → the configured
    // grant; `dark run` → NONE) before executing user code; tests run permissive.
    grantedCaps = LibExecution.Capabilities.allCaps

    accountID = None }


let rec callStackForFrame
  (vm : RT.VMState)
  (frameID : uuid)
  (soFar : RT.CallStack)
  : RT.CallStack =
  match vm.callFrames.TryGetValue frameID with
  | false, _ ->
    Exception.raiseInternal
      "Execution.callStackForFrame -- Couldn't find frame in callFrames"
      [ "frameID", string frameID ]
  | true, frame ->
    match frame.parent with
    | ValueNone -> soFar
    | ValueSome(parentFrameID, _, _) ->
      callStackForFrame vm parentFrameID (frame.executionPoint :: soFar)


let callStackFromVM (vm : RT.VMState) : RT.CallStack =
  // The nested frames go last: the stack reads outermost first, and a lambda a builtin applied ran
  // below the builtin that applied it.
  callStackForFrame vm vm.currentFrameID [] @ vm.nestedCallStack


let execute
  (exeState : RT.ExecutionState)
  (instrs : Option<tlid> * RT.Instructions)
  : Task<RT.ExecutionResult> =
  task {
    let vm = RT.VMState.create instrs
    try
      try
        // TODO: handle secrets and DBs by explicit references instead of relying on symbol table
        // vm.symbolTable <- Interpreter.withGlobals state inputVars

        let! result = Interpreter.execute exeState vm
        return Ok result

      with
      | RT.RuntimeErrorException(_threadID, rte) ->
        let callStack = callStackFromVM vm
        return Error(rte, callStack)
      | ex ->
        let metadata : Metadata =
          Exception.toMetadata ex |> List.map (fun (k, v) -> k, string v)
        do! exeState.reportException exeState vm metadata ex

        let metadata = metadata |> List.map (fun (k, v) -> k, RT.DString(string v))
        let callStack = callStackFromVM vm
        return Error(RTE.UncaughtException(ex.Message, metadata), callStack)

    finally
      // Does nothing in non-tests
      exeState.test.postTestExecutionHook exeState.test
  }

let executeExpr
  (exeState : RT.ExecutionState)
  (instrs : RT.Instructions)
  : Task<RT.ExecutionResult> =
  execute exeState (None, instrs)

let executeToplevel
  (exeState : RT.ExecutionState)
  (tlid : tlid)
  (instrs : RT.Instructions)
  : Task<RT.ExecutionResult> =
  execute exeState (Some tlid, instrs)

/// Execute an applicable (lambda or named fn) with given args in a fresh VM.
/// Lambda + package fn instruction caches live on `exeState`, so lambdas
/// created in the caller's VM remain findable here.
/// The instruction stream for applying a callable to `n` arguments, one per arity.
///
/// It is the same stream every time: `Apply` reading the callable from register 1 and the arguments
/// from 2 onwards. Building it per call meant a `LoadVal` instruction per argument, assembled with
/// list appends, purely to move values into registers that the caller can write to directly.
/// Spare VMs per thread, for `executeApplicable` to borrow.
///
/// A `ConcurrentBag` allocates a node per add, and this runs once per lambda application. A VM's
/// interpreter loop is single-threaded, so a thread-static store needs no synchronisation and no node.
///
/// A *stack*, not the single slot this used to be. The comment then said one was enough "because a
/// nested application finds it empty and builds its own", which is true and is the whole problem:
/// nesting is the common case, not the exception. `map` over a list whose lambda calls `findFirst`
/// has the outer application holding the slot for the whole traversal, so every inner one built a
/// fresh `VMState` -- thirteen dictionaries and seven arrays -- per element. It is why a native list
/// operation lost to its Dark equivalent on a five-element list while winning easily on fifty: the
/// per-call cost was fixed and large, and only long lists amortised it.
///
/// Eight deep covers the nesting real code reaches; past that it falls back to building one, which is
/// correct, just not free.
type private VMSlot() =
  static let capacity = 8

  [<System.ThreadStatic; DefaultValue>]
  static val mutable private spares : RT.VMState[]

  [<System.ThreadStatic; DefaultValue>]
  static val mutable private count : int

  static member Take() : RT.VMState voption =
    let n = VMSlot.count
    if n = 0 then
      ValueNone
    else
      let spares = VMSlot.spares
      let vm = spares[n - 1]
      // Cleared so a VM that is never borrowed again isn't held alive by the pool.
      spares[n - 1] <- Unchecked.defaultof<RT.VMState>
      VMSlot.count <- n - 1
      ValueSome vm

  static member Return(vm : RT.VMState) : unit =
    let spares =
      if obj.ReferenceEquals(VMSlot.spares, null) then
        let fresh = Array.zeroCreate capacity
        VMSlot.spares <- fresh
        fresh
      else
        VMSlot.spares

    let n = VMSlot.count
    if n < spares.Length then
      spares[n] <- vm
      VMSlot.count <- n + 1


let private applyInstrsByArity =
  System.Collections.Concurrent.ConcurrentDictionary<int, struct (RT.InstrData * int)>()

/// The `InstrData` for applying to `n` arguments, plus the register count it needs.
///
/// Cached as `InstrData` rather than `Instructions` so the array is built once ever, not converted
/// from a list on every application.
let private applyInstrsFor (argCount : int) : struct (RT.InstrData * int) =
  // `TryGetValue` first, and only then `GetOrAdd`. Passing the factory means converting an F#
  // function to a `System.Func`, and that delegate is built on every call even though the factory
  // runs only on a miss -- which is once per arity, ever, against once per lambda application.
  let mutable found = Unchecked.defaultof<struct (RT.InstrData * int)>
  if applyInstrsByArity.TryGetValue(argCount, &found) then
    found
  else

    applyInstrsByArity.GetOrAdd(
      argCount,
      fun n ->
        let argRegs = [ 2 .. n + 1 ]
        let instrData : RT.InstrData =
          { instructions =
              [| RT.Apply(0, 1, [], argRegs |> NEList.ofListUnsafe "" []) |]
            resultReg = 0 }
        struct (instrData, n + 2)
    )

/// Put the callable and its arguments straight into the root frame's registers.
let private loadApplyRegisters
  (vm : RT.VMState)
  (applicable : RT.Applicable)
  (args : NEList<RT.Dval>)
  : unit =
  let registers = vm.callFrames[vm.currentFrameID].registers
  registers[1] <- RT.DApplicable applicable
  registers[2] <- args.head
  // Head and tail directly: `NEList.toList` would allocate a list per application. The `iteri`
  // closure looks like a per-application allocation and is not one: hand-rolling the loop measured
  // neutral on a workload built to do nothing but apply lambdas, so the optimiser already elides it.
  args.tail |> List.iteri (fun i arg -> registers[i + 3] <- arg)


/// The cold path out of `executeApplicable`: something threw that was not a runtime error.
///
/// A top-level function rather than a local one, so it captures nothing and costs nothing on the
/// applications that never throw, which is all but a vanishing few.
let private uncaught
  (exeState : RT.ExecutionState)
  (vm : RT.VMState)
  (ex : exn)
  : Ply<RT.ExecutionResult> =
  uply {
    let metadata : Metadata =
      Exception.toMetadata ex |> List.map (fun (k, v) -> k, string v)
    do! exeState.reportException exeState vm metadata ex
    let metadata = metadata |> List.map (fun (k, v) -> k, RT.DString(string v))
    return Error(RTE.UncaughtException(ex.Message, metadata), callStackFromVM vm)
  }


/// A VM to apply `argCount` arguments in, borrowed from the thread's slot where one is going spare.
///
/// See `VMState.reuseFor`: a lambda applied per element of a list would otherwise build one VM per
/// element.
let private vmForApply (argCount : int) : RT.VMState =
  let struct (instrData, registerCount) = applyInstrsFor argCount

  match VMSlot.Take() with
  | ValueNone ->
    let instrs : RT.Instructions =
      { registerCount = registerCount
        instructions = List.ofArray instrData.instructions
        resultIn = 0 }
    RT.VMState.create (None, instrs)
  | ValueSome spare -> RT.VMState.reuseFor (spare, None, instrData, registerCount)


/// Apply the callable already loaded into `vm`'s registers.
let private runLoaded
  (exeState : RT.ExecutionState)
  (vm : RT.VMState)
  : Ply<RT.ExecutionResult> =
  // `Ply`, and asked synchronously first. A lambda that does not await -- which is nearly all of
  // them: an arithmetic body, a field read, a comparison -- then costs no builder at all, where
  // before it paid for a `task` state machine and the `Task` it returned. Same shape as the
  // `Ply.trySync` fast paths in the type checker.
  //
  // The success, error and exception paths used to be three local functions declared here. Each
  // captures `vm`, so all three were allocated on every application, including the overwhelmingly
  // common one that takes the synchronous success path and calls none of them. They are spelled out
  // where they are used instead: `succeeded` is three lines, and the other two are cold.
  try
    let running = Interpreter.execute exeState vm

    match Ply.trySync running with
    | ValueSome result ->
      // Only a VM that ran to completion is safe to hand back; one that raised may still hold
      // frames, and `reuseFor` assumes they have all popped.
      VMSlot.Return vm
      exeState.test.postTestExecutionHook exeState.test
      Ply(Ok result)
    | ValueNone ->
      uply {
        try
          try
            let! result = running
            VMSlot.Return vm
            return Ok result
          with
          | RT.RuntimeErrorException(_threadID, rte) ->
            return Error(rte, callStackFromVM vm)
          | ex -> return! uncaught exeState vm ex
        finally
          exeState.test.postTestExecutionHook exeState.test
      }
  with
  | RT.RuntimeErrorException(_threadID, rte) ->
    exeState.test.postTestExecutionHook exeState.test
    Ply(Error(rte, callStackFromVM vm))
  | ex ->
    uply {
      try
        return! uncaught exeState vm ex
      finally
        exeState.test.postTestExecutionHook exeState.test
    }


/// Use this when calling a Darklang callback from within a builtin.
let executeApplicable
  (exeState : RT.ExecutionState)
  (applicable : RT.Applicable)
  (args : NEList<RT.Dval>)
  : Ply<RT.ExecutionResult> =
  let vm = vmForApply (NEList.length args)
  loadApplyRegisters vm applicable args
  runLoaded exeState vm


/// Re-raise an error a lambda raised, keeping the frames it raised it in.
///
/// A builtin applying a lambda gets back `Error(rte, stack)` where `stack` covers the borrowed VM the
/// lambda ran in. Raising the error on its own -- which every caller here used to do -- threw those
/// frames away, so `List.map [1;2] (fun x -> Stdlib.Int.divide x 0)` reported a call stack that
/// stopped at the caller and never mentioned the lambda. Written in Dark it named both.
let raiseFromApplied
  (callerVm : RT.VMState)
  (rte : RTE.Error)
  (nested : RT.CallStack)
  : 'a =
  callerVm.nestedCallStack <- nested
  RT.raiseRTE callerVm.threadID rte


/// One argument, without the `NEList` holding it. See `executeApplicable2`.
let executeApplicable1
  (exeState : RT.ExecutionState)
  (applicable : RT.Applicable)
  (arg : RT.Dval)
  : Ply<RT.ExecutionResult> =
  let vm = vmForApply 1
  let registers = vm.callFrames[vm.currentFrameID].registers
  registers[1] <- RT.DApplicable applicable
  registers[2] <- arg
  runLoaded exeState vm


/// Two arguments, without the `NEList` holding them.
///
/// `executeApplicable` is the general form and the one to reach for. This exists because a builtin
/// folding a list applies a two-argument lambda once per element, and building an `NEList` for each
/// costs a cons and a record per element -- together the largest allocation on that path after the
/// interpreter's own.
let executeApplicable2
  (exeState : RT.ExecutionState)
  (applicable : RT.Applicable)
  (arg1 : RT.Dval)
  (arg2 : RT.Dval)
  : Ply<RT.ExecutionResult> =
  let vm = vmForApply 2
  let registers = vm.callFrames[vm.currentFrameID].registers
  registers[1] <- RT.DApplicable applicable
  registers[2] <- arg1
  registers[3] <- arg2
  runLoaded exeState vm


let executeFunction
  (exeState : RT.ExecutionState)
  (name : RT.FQFnName.FQFnName)
  (typeArgs : List<RT.TypeReference>)
  (args : NEList<RT.Dval>)
  : Task<RT.ExecutionResult> =
  let resultReg, rc = 0, 1

  let fnInstr, fnReg, rc =
    let namedFn : RT.ApplicableNamedFn =
      { name = name
        typeSymbolTable = RT.TST.empty
        typeArgs = typeArgs
        argsSoFar = [] }
    let applicable = RT.DApplicable(RT.AppNamedFn namedFn)
    RT.LoadVal(rc, applicable), rc, rc + 1

  let argInstrs, argRegs, rc =
    args
    |> NEList.fold
      (fun (instrs, argRegs, rc) arg ->
        instrs @ [ RT.LoadVal(rc, arg) ], argRegs @ [ rc ], rc + 1)
      ([], [], rc)

  let applyInstr =
    RT.Apply(resultReg, fnReg, typeArgs, argRegs |> NEList.ofListUnsafe "" [])

  let instrs : RT.Instructions =
    { registerCount = rc
      instructions = [ fnInstr ] @ argInstrs @ [ applyInstr ]
      resultIn = 0 }
  executeExpr exeState instrs


let runtimeErrorToString
  (state : RT.ExecutionState)
  (rte : RT.RuntimeError.Error)
  : Task<RT.ExecutionResult> =
  task {
    let fnName =
      RT.FQFnName.fqPackage (
        PackageRefs.Fn.PrettyPrinter.RuntimeTypes.RuntimeError.toString ()
      )
    let args =
      NEList.ofList (RT.DUuid state.branchId) [ RT2DT.RuntimeError.toDT rte ]
    return! executeFunction state fnName [] args
  }

/// Fallback for when a pretty printer call fails: the error it raised, then the raw value.
let private prettyPrintFallback
  (label : string)
  (raw : obj)
  (result : RT.ExecutionResult)
  : string =
  match result with
  | Error(rte, _callStack) ->
    $"<pretty-print failed for {label}: {rte}\n  value: {raw}>"
  | Ok other ->
    $"<pretty-print failed for {label}: printer returned {other}\n  value: {raw}>"

let fnNameToString
  (state : RT.ExecutionState)
  (name : RT.FQFnName.FQFnName)
  : Task<string> =
  task {
    let fnName =
      RT.FQFnName.fqPackage (PackageRefs.Fn.PrettyPrinter.RuntimeTypes.fnName ())
    let args = NEList.ofList (RT.DUuid state.branchId) [ RT2DT.FQFnName.toDT name ]
    match! executeFunction state fnName [] args with
    | Ok(RT.DString s) -> return s
    | result -> return prettyPrintFallback "fnName" name result
  }


let dvalToRepr (state : RT.ExecutionState) (dval : RT.Dval) : Task<string> =
  task {
    let fnName =
      RT.FQFnName.fqPackage (PackageRefs.Fn.PrettyPrinter.RuntimeTypes.dval ())
    let args = NEList.ofList (RT.DUuid state.branchId) [ RT2DT.Dval.toDT dval ]
    match! executeFunction state fnName [] args with
    | Ok(RT.DString s) -> return s
    | result -> return prettyPrintFallback "dval" dval result
  }


/// Like `dvalToRepr`, but laid out for a line `width` columns wide and painted for a terminal.
///
/// The width is passed in rather than looked up, because the printers are deliberately unable to read
/// the terminal: that is what keeps rendering reproducible in tests and lets a pane pass its own width
/// rather than the screen's.
///
/// Goes through the CLI's own `Terminal.renderValue`, which supplies the palette, so nothing here has
/// to know what a color is or build a `Palette` to ask for one.
let dvalToReprForTerminal
  (state : RT.ExecutionState)
  (width : int)
  (color : bool)
  (currentModule : List<string>)
  (dval : RT.Dval)
  : Task<string> =
  task {
    let fnName = RT.FQFnName.fqPackage (PackageRefs.Fn.Cli.renderValue ())
    let currentModule = currentModule |> List.map RT.DString |> Dval.list RT.KTString
    let args =
      NEList.ofList
        (RT.DUuid state.branchId)
        [ Dval.int (bigint width)
          RT.DBool color
          currentModule
          RT2DT.Dval.toDT dval ]
    match! executeFunction state fnName [] args with
    | Ok(RT.DString s) -> return s
    | result -> return prettyPrintFallback "dval" dval result
  }


let typeRefToString
  (state : RT.ExecutionState)
  (typeRef : RT.TypeReference)
  : Task<string> =
  task {
    let fnName =
      RT.FQFnName.fqPackage (
        PackageRefs.Fn.PrettyPrinter.RuntimeTypes.typeReference ()
      )
    let args =
      NEList.ofList (RT.DUuid state.branchId) [ RT2DT.TypeReference.toDT typeRef ]
    match! executeFunction state fnName [] args with
    | Ok(RT.DString s) -> return s
    | result -> return prettyPrintFallback "typeRef" typeRef result
  }

let dvalToTypeName (state : RT.ExecutionState) (dval : RT.Dval) : Task<string> =
  task {
    let fnName =
      RT.FQFnName.fqPackage (
        PackageRefs.Fn.PrettyPrinter.RuntimeTypes.Dval.valueTypeName ()
      )
    let args = NEList.ofList (RT.DUuid state.branchId) [ RT2DT.Dval.toDT dval ]
    match! executeFunction state fnName [] args with
    | Ok(RT.DString s) -> return s
    | result -> return prettyPrintFallback "typeName" dval result
  }



let executionPointToString
  (state : RT.ExecutionState)
  (ep : RT.ExecutionPoint)
  : Ply<string> =
  uply {
    // CLEANUP improve here
    // let handleFn (fn : Option<RT.PackageFn.PackageFn>) : Ply<string> =
    //   uply {
    //     match fn with
    //     | None -> return $"<Couldn't find package function {fn.id}>"
    //     | Some fn ->
    //       let fnName = string fn.id
    //       let! exprString = exprString state fn.body exprId
    //       return fnName + ": " + exprString
    //   }

    match ep with
    | RT.Source -> return "Source"
    | RT.Function(RT.FQFnName.Package _ as name) ->
      let! prettyName = fnNameToString state name
      return $"Package Function {prettyName}"
    | RT.Function(RT.FQFnName.Builtin fnName) ->
      return $"Builtin Function {fnName.name}" // TODO actually fetch the fn, etc
    | RT.Lambda(_parent, exprId) -> return ("Lambda " + string exprId)
  }


/// CLEANUPs
/// - move this impl to darklang
/// - consider accepting a VMState rather than the CallStack
/// - generally tidy the output here
let callStackString
  (state : RT.ExecutionState)
  (callStack : RT.CallStack)
  : Ply<string> =
  uply {
    // First, convert all execution points to strings
    let! stringParts =
      Ply.List.mapSequentially (fun ep -> executionPointToString state ep) callStack

    // Group consecutive identical entries with counts
    let rec groupConsecutive acc current count remaining =
      match remaining with
      | [] ->
        // Add the final group
        let countStr = if count = 1 then "" else $" (×{count})"
        List.rev ((current + countStr) :: acc)
      | head :: tail ->
        if head = current then
          // Same as current, increment count
          groupConsecutive acc current (count + 1) tail
        else
          // Different, add current group and start new one
          let countStr = if count = 1 then "" else $" (×{count})"
          groupConsecutive ((current + countStr) :: acc) head 1 tail

    let groupedParts =
      match stringParts with
      | [] -> []
      | head :: tail -> groupConsecutive [] head 1 tail

    // Build the final string
    let result =
      groupedParts
      |> List.fold
        (fun acc part -> $"{acc}\n- {part}")
        "Call stack (last call at bottom):"

    return result
  }




let rec rteToString
  (rteToDval : RT.RuntimeError.Error -> RT.Dval)
  (state : RT.ExecutionState)
  (rte : RT.RuntimeError.Error)
  : Ply<string> =
  let r = rteToString rteToDval state
  uply {
    let errorMessageFn =
      RT.FQFnName.fqPackage (
        PackageRefs.Fn.PrettyPrinter.RuntimeTypes.RuntimeError.toErrorMessage ()
      )

    let rteDval = rteToDval rte

    let! rteMessage =
      executeFunction
        state
        errorMessageFn
        []
        (NEList.ofList (RT.DUuid state.branchId) [ rteDval ])

    match rteMessage with
    | Ok(RT.DString msg) -> return msg
    | Ok(other) -> return prettyPrintFallback "rteToString" other rteMessage
    | Error(rte, _cs) ->
      debuG "Error converting RTE to string" rte
      return! r rte
  }
