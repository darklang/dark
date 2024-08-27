/// Interprets Dark instructions resulting in (tasks of) Dvals
module LibExecution.Interpreter

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine.Unsafe

open Prelude
open RuntimeTypes
module RTE = RuntimeError
module VT = ValueType


let rec checkAndExtractLetPattern
  (pat : LetPattern)
  (dv : Dval)
  : bool * List<string * Dval> =
  let r = checkAndExtractLetPattern

  let rec rList pats items =
    match pats, items with
    | [], [] -> true, []
    | [], _ -> false, []
    | _, [] -> false, []
    | pat :: otherPats, item :: items ->
      let matches, vars = r pat item
      if matches then
        let matchesRest, varsRest = rList otherPats items
        if matchesRest then true, vars @ varsRest else false, []
      else
        false, []

  match pat, dv with
  | LPVariable name, dv -> true, [ (name, dv) ]
  | LPUnit, DUnit -> true, []
  | LPTuple(first, second, theRest), DTuple(firstVal, secondVal, theRestVal) ->
    match r first firstVal, r second secondVal with
    | (true, varsFirst), (true, varsSecond) ->
      match rList theRest theRestVal with
      | true, varsRest -> true, varsFirst @ varsSecond @ varsRest
      | false, _ -> false, []
    | _ -> false, []
  | _ -> false, []


let rec checkAndExtractMatchPattern
  (pat : MatchPattern)
  (dv : Dval)
  : bool * List<string * Dval> =
  let r = checkAndExtractMatchPattern

  let rec rList pats items =
    match pats, items with
    | [], [] -> true, []
    | [], _ -> false, []
    | _, [] -> false, []
    | pat :: otherPats, item :: items ->
      let matches, vars = r pat item
      if matches then
        let matchesRest, varsRest = rList otherPats items
        if matchesRest then true, vars @ varsRest else false, []
      else
        false, []

  match pat, dv with
  | MPVariable name, dv -> true, [ (name, dv) ]

  | MPUnit, DUnit -> true, []
  | MPBool l, DBool r -> l = r, []
  | MPInt8 l, DInt8 r -> l = r, []
  | MPUInt8 l, DUInt8 r -> l = r, []
  | MPInt16 l, DInt16 r -> l = r, []
  | MPUInt16 l, DUInt16 r -> l = r, []
  | MPInt32 l, DInt32 r -> l = r, []
  | MPUInt32 l, DUInt32 r -> l = r, []
  | MPInt64 l, DInt64 r -> l = r, []
  | MPUInt64 l, DUInt64 r -> l = r, []
  | MPInt128 l, DInt128 r -> l = r, []
  | MPUInt128 l, DUInt128 r -> l = r, []
  | MPFloat l, DFloat r -> l = r, []
  | MPChar l, DChar r -> l = r, []
  | MPString l, DString r -> l = r, []

  | MPList pats, DList(_, items) -> rList pats items

  | MPListCons(head, tail), DList(vt, items) ->
    match items with
    | [] -> false, []
    | headItem :: tailItems ->
      let matchesHead, varsHead = r head headItem
      if matchesHead then
        let matchesTail, varsTail = r tail (DList(vt, tailItems))
        if matchesTail then true, varsHead @ varsTail else false, []
      else
        false, []

  | MPTuple(first, second, theRest), DTuple(firstVal, secondVal, theRestVal) ->
    match r first firstVal, r second secondVal with
    | (true, varsFirst), (true, varsSecond) ->
      match rList theRest theRestVal with
      | true, varsRest -> true, varsFirst @ varsSecond @ varsRest
      | false, _ -> false, []
    | _ -> false, []

  // Dval didn't match the pattern even in a basic sense
  | _ -> false, []


/// TODO: don't pass ExecutionState around so much?
/// The parts that change, (e.g. `st` and `tst`) should probably all be part of VMState
///
/// Maybe rename ExecutionState to something else
/// , like ExecutionContext or Execution
///
/// TODO potentially make this a loop instead of recursive
let rec private execute (exeState : ExecutionState) (vm : VMState) : Ply<Dval> =
  uply {
    let mutable counter = vm.pc // what instruction (by index) we're on

    let raiseRTE rte = raiseRTE vm.callStack rte

    while counter < vm.instructions.Length do

      match vm.instructions[counter] with

      // == Simple register operations ==
      | LoadVal(reg, value) ->
        vm.registers[reg] <- value
        counter <- counter + 1

      | CopyVal(copyTo, copyFrom) ->
        vm.registers[copyTo] <- vm.registers[copyFrom]
        counter <- counter + 1


      // == Working with Variables ==
      | GetVar(loadTo, varName) ->
        match Map.find varName vm.symbolTable with
        | Some value ->
          vm.registers[loadTo] <- value
          counter <- counter + 1
        | None -> raiseRTE (RTE.Error.VariableNotFound varName)

      | CheckLetPatternAndExtractVars(valueReg, pat) ->
        let dv = vm.registers[valueReg]
        let matches, vars = checkAndExtractLetPattern pat dv

        if matches then
          vm.symbolTable <-
            List.fold
              (fun symbolTable (varName, value) -> Map.add varName value symbolTable)
              vm.symbolTable
              vars
          counter <- counter + 1
        else
          raiseRTE (RTE.Let(RTE.Lets.PatternDoesNotMatch(dv, pat)))

      // == Working with Basic Types ==
      | CreateString(targetReg, segments) ->
        let sb = new System.Text.StringBuilder()

        segments
        |> List.iter (fun seg ->
          match seg with
          | StringSegment.Text s -> sb.Append s |> ignore<System.Text.StringBuilder>
          | StringSegment.Interpolated reg ->
            match vm.registers[reg] with
            | DString s -> sb.Append s |> ignore<System.Text.StringBuilder>
            | _ -> raiseRTE (RTE.String RTE.Strings.Error.InvalidStringAppend))

        vm.registers[targetReg] <- DString(sb.ToString())
        counter <- counter + 1


      // == Flow Control ==
      // -- Jumps --
      | JumpBy jumpBy -> counter <- counter + jumpBy + 1

      | JumpByIfFalse(jumpBy, condReg) ->
        match vm.registers[condReg] with
        | DBool false -> counter <- counter + jumpBy + 1
        | DBool true -> counter <- counter + 1
        | dv ->
          let vt = Dval.toValueType dv
          raiseRTE (RTE.Bool(RTE.Bools.ConditionRequiresBool(vt, dv)))

      // -- Match --
      | CheckMatchPatternAndExtractVars(valueReg, pat, failJump) ->
        let matches, vars = checkAndExtractMatchPattern pat vm.registers[valueReg]

        if matches then
          vm.symbolTable <-
            List.fold
              (fun symbolTable (varName, value) -> Map.add varName value symbolTable)
              vm.symbolTable
              vars
          counter <- counter + 1
        else
          counter <- counter + failJump + 1

      | MatchUnmatched -> raiseRTE RTE.MatchUnmatched


      // == Working with Collections ==
      | CreateList(listReg, itemsToAddRegs) ->
        // CLEANUP reference registers directly in DvalCreator.list,
        // so we don't have to copy things
        let itemsToAdd = itemsToAddRegs |> List.map (fun r -> vm.registers[r])
        vm.registers[listReg] <-
          TypeChecker.DvalCreator.list vm.callStack VT.unknown itemsToAdd
        counter <- counter + 1

      | CreateDict(dictReg, entries) ->
        // CLEANUP reference registers directly in DvalCreator.dict,
        // so we don't have to copy things
        let entries =
          entries |> List.map (fun (key, valueReg) -> (key, vm.registers[valueReg]))
        vm.registers[dictReg] <-
          TypeChecker.DvalCreator.dict vm.callStack VT.unknown entries
        counter <- counter + 1

      | CreateTuple(tupleReg, firstReg, secondReg, theRestRegs) ->
        let first = vm.registers[firstReg]
        let second = vm.registers[secondReg]
        let theRest = theRestRegs |> List.map (fun r -> vm.registers[r])
        vm.registers[tupleReg] <- DTuple(first, second, theRest)
        counter <- counter + 1


      // == Working with Custom Data ==
      // -- Records --
      | CreateRecord(recordReg, typeName, typeArgs, fields) ->
        let fields =
          fields |> List.map (fun (name, valueReg) -> (name, vm.registers[valueReg]))

        let! record =
          TypeChecker.DvalCreator.record
            vm.callStack
            exeState.types
            typeName
            typeArgs
            fields

        vm.registers[recordReg] <- record
        counter <- counter + 1

      // | CloneRecordWithUpdates(targetReg, originalRecordReg, updates) ->
      //   let originalRecord = vm.registers[originalRecordReg]
      //   let updates =
      //     updates
      //     |> List.map (fun (fieldName, valueReg) ->
      //       (fieldName, vm.registers[valueReg]))
      //   let updatedRecord =
      //     TypeChecker.DvalCreator.record
      //       exeState.tracing.callStack
      //       typeName
      //       typeArgs
      //       updates

      //   vm.registers[targetReg] <- updatedRecord
      //   counter <- counter + 1

      | GetRecordField(targetReg, recordReg, fieldName) ->
        match vm.registers[recordReg] with
        | DRecord(_, _, _, fields) ->
          match Map.find fieldName fields with
          | Some value ->
            vm.registers[targetReg] <- value
            counter <- counter + 1
          | None ->
            RTE.Records.FieldAccessFieldNotFound fieldName |> RTE.Record |> raiseRTE
        | dv ->
          RTE.Records.FieldAccessNotRecord(Dval.toValueType dv)
          |> RTE.Record
          |> raiseRTE

      // -- Enums --
      | CreateEnum(enumReg, typeName, _typeArgs, caseName, fields) ->
        // TODO: safe dval creation
        let fields = fields |> List.map (fun (valueReg) -> vm.registers[valueReg])
        vm.registers[enumReg] <- DEnum(typeName, typeName, [], caseName, fields)
        counter <- counter + 1


      // == Working with things that Apply (like fns, lambdas) ==
      // // `add (increment 1L) (3L)` and store results in `putResultIn`
      // // At this point, the 'increment' has already been evaluated.
      // // But maybe that's something we should change, (CLEANUP)
      // // so that we don't execute things until they're needed
      // | Apply(putResultIn, thingToCallReg, typeArgs, argRegs) ->
      //   // should we instead pass in register indices? probably...
      //   let args = argRegs |> NEList.map (fun r -> vm.registers[r])
      //   //debuG "args" (NEList.length args)
      //   let thingToCall = vm.registers[thingToCallReg]
      //   let! result = call exeState vm thingToCall typeArgs args
      //   vm.registers[putResultIn] <- result
      //   counter <- counter + 1


      | RaiseNRE nre -> raiseRTE (RTE.NameResolution nre)


    // If we've reached the end of the instructions, return the result
    return vm.registers[vm.resultReg]
  }


// and call
//   (exeState : ExecutionState)
//   (vmState : VMState)
//   (thingToCall : Dval)
//   (typeArgs : List<TypeReference>)
//   (args : NEList<Dval>)
//   : Ply<Dval> =
//   uply {
//     match thingToCall with
//     | DFnVal(NamedFn fnName) ->
//       let! fn =
//         match fnName with
//         | FQFnName.Builtin std ->
//           Map.find std exeState.fns.builtIn |> Option.map builtInFnToFn |> Ply

//         | FQFnName.Package pkg ->
//           uply {
//             let! fn = exeState.fns.package pkg
//             return Option.map packageFnToFn fn
//           }

//       match fn with
//       | Some fn ->
//         // let expectedTypeParams = List.length fn.typeParams
//         // let expectedArgs = NEList.length fn.parameters

//         // let actualTypeArgs = List.length typeArgs
//         // let actualArgs = NEList.length args

//         // if expectedTypeParams <> actualTypeArgs || expectedArgs <> actualArgs then
//         //   ExecutionError.raise
//         //     state.tracing.callStack
//         //     (ExecutionError.WrongNumberOfFnArgs(
//         //       fnToCall,
//         //       expectedTypeParams,
//         //       expectedArgs,
//         //       actualTypeArgs,
//         //       actualArgs
//         //     ))

//         let vmState =
//           let boundArgs =
//             NEList.map2
//               (fun (p : Param) actual -> (p.name, actual))
//               fn.parameters
//               args
//             |> NEList.toList
//             |> Map
//           { vmState with
//               symbolTable = Map.mergeFavoringRight vmState.symbolTable boundArgs }

//         let vmState =
//           let newlyBoundTypeArgs = List.zip fn.typeParams typeArgs |> Map
//           { vmState with
//               typeSymbolTable =
//                 Map.mergeFavoringRight vmState.typeSymbolTable newlyBoundTypeArgs }

//         return! execFn exeState vmState fnName fn typeArgs args

//       | None ->
//         // Functions which aren't available in the runtime (for whatever reason)
//         // may have results available in traces. (use case: inspecting a cloud-run trace locally)
//         let fnResult =
//           exeState.tracing.loadFnResult
//             (exeState.tracing.callStack.lastCalled, fnName)
//             args

//         match fnResult with
//         | Some(result, _ts) -> return result
//         | None ->
//           return
//             raiseRTE
//               exeState.tracing.callStack
//               (RuntimeError.oldError
//                 $"Function {FQFnName.toString fnName} is not found")

//     | _ ->
//       debuG "thingToCall" thingToCall
//       return DUnit // TODO
//   }

// and execFn
//   (exeState : ExecutionState)
//   (vmState : VMState)
//   (fnDesc : FQFnName.FQFnName)
//   (fn : Fn)
//   (typeArgs : List<TypeReference>)
//   (args : NEList<Dval>)
//   : DvalTask =
//   uply {
//     let typeArgsResolvedInFn = List.zip fn.typeParams typeArgs |> Map
//     let typeSymbolTable =
//       Map.mergeFavoringRight vmState.typeSymbolTable typeArgsResolvedInFn

//     match! TypeChecker.checkFunctionCall exeState.types typeSymbolTable fn args with
//     | Error rte -> return raiseRTE exeState.tracing.callStack rte
//     | Ok() ->
//       let! result =
//         match fn.fn with
//         | BuiltInFunction f ->
//           let executionPoint = ExecutionPoint.Function fn.name

//           exeState.tracing.traceExecutionPoint executionPoint

//           let exeState =
//             { exeState with tracing.callStack.lastCalled = (executionPoint, None) }

//           uply {
//             let! result =
//               uply {
//                 try
//                   return! f (exeState, vmState, typeArgs, NEList.toList args)
//                 with e ->
//                   match e with
//                   | RuntimeErrorException(None, rte) ->
//                     // Sometimes it's awkward, in a Builtin fn impl, to pass around the callStack
//                     // So we catch the exception here and add the callStack to it so it's handy in error-reporting
//                     return raiseRTE exeState.tracing.callStack rte

//                   | RuntimeErrorException _ -> return Exception.reraise e

//                   | e ->
//                     let context : Metadata =
//                       [ "fn", fnDesc; "args", args; "typeArgs", typeArgs; "id", id ]
//                     exeState.reportException exeState context e
//                     // These are arbitrary errors, and could include sensitive
//                     // information, so best not to show it to the user. If we'd
//                     // like to show it to the user, we should catch it where it happens
//                     // and give them a known safe error via a RuntimeError
//                     return
//                       raiseRTE
//                         exeState.tracing.callStack
//                         (RuntimeError.oldError "Unknown error")
//               }

//             if fn.previewable <> Pure then
//               // TODO same thing here -- shouldn't require ourselves to pass in lastCalled - `tracing` should just get access to it underneath
//               exeState.tracing.storeFnResult
//                 (exeState.tracing.callStack.lastCalled, fnDesc)
//                 args
//                 result

//             return result
//           }

//         | PackageFunction(_id, _instructionsWithContext) ->
//           //let _registersNeeded, instructions, resultReg = _instructionsWithContext
//           // // maybe this should instead be something like `state.tracing.tracePackageFnCall tlid`?
//           // // and the `caller` would be updated by that function? (maybe `caller` is a read-only thing.)
//           // let executionPoint = ExecutionPoint.Function(FQFnName.Package id)

//           // state.tracing.traceExecutionPoint executionPoint

//           // // let state =
//           // //   { state with
//           // //       tracing.callStack.lastCalled = (executionPoint, Some(Expr.toID body)) }

//           // and how can we pass the args in?
//           // maybe fns need some LoadVal instructions frontloaded or something? hmm.
//           //eval state instructions resultReg
//           Ply DUnit // TODO

//       match!
//         TypeChecker.checkFunctionReturnType exeState.types typeSymbolTable fn result
//       with
//       | Error rte -> return raiseRTE exeState.tracing.callStack rte
//       | Ok() -> return result
//   }



and eval (exeState : ExecutionState) (vmState : VMState) : Ply<Dval> =
  execute exeState vmState
