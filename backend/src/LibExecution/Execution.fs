module LibExecution.Execution

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = RuntimeTypes
module RTE = RT.RuntimeError
module RT2DT = RuntimeTypesToDarkTypes

let noTracing : RT.Tracing.Tracing =
  { traceDval = fun _ _ -> ()
    traceExecutionPoint = fun _ -> ()
    loadFnResult = fun _ _ -> None
    storeFnResult = fun _ _ _ -> () }

let noTestContext : RT.TestContext =
  { sideEffectCount = 0

    exceptionReports = []
    expectedExceptionCount = 0
    postTestExecutionHook = fun _ -> () }

let createState
  (builtins : RT.Builtins)
  (packageManager : RT.PackageManager)
  (tracing : RT.Tracing.Tracing)
  (reportException : RT.ExceptionReporter)
  (notify : RT.Notifier)
  (program : RT.Program)
  : RT.ExecutionState =
  { tracing = tracing
    test = noTestContext
    reportException = reportException
    notify = notify

    program = program

    types = { package = packageManager.getType }
    fns = { builtIn = builtins.fns; package = packageManager.getFn }
    constants =
      { builtIn = builtins.constants; package = packageManager.getConstant } }


let rec callStackForFrame
  (vm : RT.VMState)
  (frameID : uuid)
  (soFar : RT.CallStack)
  : RT.CallStack =
  match vm.callFrames |> Map.find frameID with
  | None ->
    Exception.raiseInternal
      "Execution.callStackForFrame -- Couldn't find frame in callFrames"
      [ "frameID", string frameID ]
  | Some frame ->
    match frame.parent with
    | None -> soFar
    | Some(parentFrameID, _, _) ->
      callStackForFrame vm parentFrameID (frame.executionPoint :: soFar)


let callStackFromVM (vm : RT.VMState) : RT.CallStack =
  callStackForFrame vm vm.currentFrameID []


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
        exeState.reportException exeState vm metadata ex

        let metadata = metadata |> List.map (fun (k, v) -> k, RT.DString(string v))
        return
          (RTE.UncaughtException(ex.Message, metadata)) |> RT.raiseRTE vm.threadID

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
        typeSymbolTable = Map.empty
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
      RT.FQFnName.fqPackage
        PackageIDs.Fn.PrettyPrinter.RuntimeTypes.RuntimeError.toString
    let args = NEList.singleton (RT2DT.RuntimeError.toDT rte)
    return! executeFunction state fnName [] args
  }


// let exprString
//   (state : RT.ExecutionState)
//   (expr : RT.Expr)
//   (id : Option<id>)
//   : Ply<string> =
//   match id with
//   | None -> Ply "Unknown Expr"
//   | Some id ->
//     let mutable foundExpr = None

//     RuntimeTypesAst.preTraversal
//       (fun expr ->
//         if RT.Expr.toID expr = id then foundExpr <- Some expr
//         expr)
//       identity
//       identity
//       identity
//       identity
//       identity
//       identity
//       expr
//     |> ignore<RT.Expr>

//     let prettyPrint (expr : RT.Expr) : Ply<string> =
//       uply {
//         let fnName =
//           RT.FQFnName.fqPackage PackageIDs.Fn.PrettyPrinter.RuntimeTypes.expr
//         let args = NEList.singleton (RuntimeTypesToDarkTypes.Expr.toDT expr)

//         match! executeFunction state fnName [] args with
//         | Ok(RT.DString s) -> return s
//         | _ -> return string expr
//       }

//     match foundExpr with
//     | None ->
//       uply {
//         let! pretty = prettyPrint expr
//         return $"Root Expr:\n{pretty}"
//       }
//     | Some expr -> prettyPrint expr


let executionPointToString
  (_state : RT.ExecutionState)
  (ep : RT.ExecutionPoint)
  : string =
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
  | RT.Source -> "Source"
  | RT.Function(RT.FQFnName.Package id) -> $"Package Function {id}"
  | RT.Function(RT.FQFnName.Builtin fnName) -> $"Builtin Function {fnName}" // TODO actually fetch the fn, etc
  | RT.Lambda(_parent, exprId) -> "Lambda " + string exprId


/// CLEANUPs
/// - move this impl to darklang
/// - consider accepting a VMState rather than the CallStack
/// - generally tidy the output here
let callStackString
  (state : RT.ExecutionState)
  (callStack : RT.CallStack)
  : Ply<string> =
  uply {
    return!
      Ply.List.foldSequentially
        (fun acc executionPoint ->
          uply {
            let part = executionPointToString state executionPoint
            return $"{acc}\n- {part}"
          })
        "Call stack (last call at bottom):"
        callStack
  }




// /// Return a function to trace TLIDs (add it to state via
// /// state.tracing.traceExecutionPoint), and a mutable set which updates when the
// /// traceFn is used
// /// TRACINGTODO
// let traceTLIDs () : HashSet.HashSet<tlid> * RT.TraceExecutionPoint =
//   let touchedTLIDs = HashSet.empty ()
//   let traceExecutionPoint tlid : unit = HashSet.add tlid touchedTLIDs
//   (touchedTLIDs, traceExecutionPoint)

/// Return a function to trace Dvals (add it to state via
/// state.tracing.traceDval), and a mutable dictionary which updates when the
/// traceFn is used
let traceDvals () : Dictionary.T<id, RT.Dval> * RT.Tracing.TraceDval =
  let results = Dictionary.empty ()

  let trace (id : id) (dval : RT.Dval) : unit =
    // Overwrites if present, which is what we want
    results[id] <- dval

  (results, trace)


let rec rteToString
  (rteToDval : RT.RuntimeError.Error -> RT.Dval)
  (state : RT.ExecutionState)
  (rte : RT.RuntimeError.Error)
  : Ply<string> =
  uply {
    let errorMessageFn =
      RT.FQFnName.fqPackage
        PackageIDs.Fn.PrettyPrinter.RuntimeTypes.RuntimeError.toErrorMessage

    let rteDval = rteToDval rte

    let! rteMessage =
      executeFunction state errorMessageFn [] (NEList.ofList rteDval [])

    match rteMessage with
    | Ok(RT.DString msg) -> return msg
    | Ok(other) -> return string other
    | Error(rte, _cs) ->
      debuG "Error converting RTE to string" rte
      return! rteToString rteToDval state rte
  }
