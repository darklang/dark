/// Interprets Dark instructions resulting in (tasks of) Dvals
module LibExecution.Interpreter

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine.Unsafe

open Prelude
open RuntimeTypes
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
let rec private execute
  (exeState : ExecutionState)
  (initialVmState : VMState)
  : Ply<Dval> =
  uply {
    let mutable vmState = initialVmState
    let mutable counter = 0 // what instruction (by index) we're on

    // if we encounter a runtime error, we store it here and then `raise` it at the end
    let mutable rte : Option<RuntimeError> = None

    while counter < vmState.instructions.Length && Option.isNone rte do
      let instruction = vmState.instructions[counter]

      match instruction with
      // put a static Dval into a register
      | LoadVal(reg, value) ->
        vmState.registers[reg] <- value
        counter <- counter + 1

      // `let x = 1`
      | SetVar(varName, loadFrom) ->
        let value = vmState.registers[loadFrom]
        vmState <-
          { vmState with symbolTable = Map.add varName value vmState.symbolTable }
        counter <- counter + 1

      // later, `x`
      | GetVar(loadTo, varName) ->
        let value =
          Map.find varName vmState.symbolTable
          // TODO: handle missing variable
          //return errStr callStack $"There is no variable named: {name}"
          |> Option.defaultValue DUnit

        vmState.registers[loadTo] <- value

        counter <- counter + 1


      // `add (increment 1L) (3L)` and store results in `putResultIn`
      // At this point, the 'increment' has already been evaluated.
      // But maybe that's something we should change, (CLEANUP)
      // so that we don't execute things until they're needed
      | Apply(putResultIn, thingToCallReg, typeArgs, argRegs) ->
        // should we instead pass in register indices? probably...
        let args = argRegs |> NEList.map (fun r -> vmState.registers[r])
        //debuG "args" (NEList.length args)
        let thingToCall = vmState.registers[thingToCallReg]
        let! result = call exeState vmState thingToCall typeArgs args
        vmState.registers[putResultIn] <- result
        counter <- counter + 1

      | CreateList(listReg, itemsToAddRegs) ->
        // CLEANUP reference registers directly in DvalCreator.list,
        // so we don't have to copy things
        let itemsToAdd = itemsToAddRegs |> List.map (fun r -> vmState.registers[r])
        vmState.registers[listReg] <-
          TypeChecker.DvalCreator.list
            exeState.tracing.callStack
            VT.unknown
            itemsToAdd
        counter <- counter + 1

      | CreateDict(dictReg, entries) ->
        // CLEANUP reference registers directly in DvalCreator.dict,
        // so we don't have to copy things
        let entries =
          entries
          |> List.map (fun (key, valueReg) -> (key, vmState.registers[valueReg]))
        vmState.registers[dictReg] <- TypeChecker.DvalCreator.dict VT.unknown entries
        counter <- counter + 1

      | CreateTuple(tupleReg, firstReg, secondReg, theRestRegs) ->
        let first = vmState.registers[firstReg]
        let second = vmState.registers[secondReg]
        let theRest = theRestRegs |> List.map (fun r -> vmState.registers[r])
        vmState.registers[tupleReg] <- DTuple(first, second, theRest)
        counter <- counter + 1


      // I'm not sure, but it also feels like string-creation doesn't need to be so many
      // instructions. Maybe we should just have a CreateString instruction.
      // Maybe that's a tad more complicated because of interpolation... but maybe not actually.
      // If CreateString just references a list of registers, then the interpolation is already
      // done by the time we get to CreateString.
      // I don't think we need to worry about checking "is this string part really a string"
      // before we get to CreateString.
      // Oh, that said - if there's nested string interpolation (if that's legal?), would that
      // result in nested CreateString instructions? Write out an example.
      // OK did some quick search and it seems no language really allows nested string interpolation.
      // So we're probably fine.
      // That said, let's also consider the _normal_ case of a String with a simple StringText or StringInterpolation
      // segment - this shouldn't result in many instructions.
      // CreateString itself could contain a list of Text and Interpolation segments, where Interpolation
      // segments just refer to a register with some (supposed) string value -- and we only have to cehck those.
      | AppendString(targetReg, sourceReg) ->
        match vmState.registers[targetReg], vmState.registers[sourceReg] with
        | DString target, DString source ->
          vmState.registers[targetReg] <- DString(target + source)
          counter <- counter + 1
        | _, _ ->
          // TODO
          rte <- Some(RuntimeError.oldError "Error: Invalid string-append attempt")


      | JumpByIfFalse(jumpBy, condReg) ->
        match vmState.registers[condReg] with
        | DBool false -> counter <- counter + jumpBy + 1
        | DBool true -> counter <- counter + 1
        | _ ->
          // TODO
          rte <-
            Some(RuntimeError.oldError "Error: Jump condition must be a boolean")

      | JumpBy jumpBy -> counter <- counter + jumpBy + 1


      | CopyVal(copyTo, copyFrom) ->
        vmState.registers[copyTo] <- vmState.registers[copyFrom]
        counter <- counter + 1

      | CheckMatchPatternAndExtractVars(valueReg, pat, failJump) ->
        let matches, vars =
          checkAndExtractMatchPattern pat vmState.registers[valueReg]

        if matches then
          vmState <-
            vars
            |> List.fold
              (fun vmState (varName, value) ->
                { vmState with
                    symbolTable = Map.add varName value vmState.symbolTable })
              vmState
          counter <- counter + 1
        else
          counter <- counter + failJump + 1


      | CheckLetPatternAndExtractVars(valueReg, pat) ->
        let matches, vars = checkAndExtractLetPattern pat vmState.registers[valueReg]

        if matches then
          vmState <-
            vars
            |> List.fold
              (fun vmState (varName, value) ->
                { vmState with
                    symbolTable = Map.add varName value vmState.symbolTable })
              vmState
          counter <- counter + 1
        else
          rte <- Some(RuntimeError.oldError "Let Pattern did not match")

      | Fail _rte -> rte <- Some(RuntimeError.oldError "TODO")

      | MatchUnmatched -> rte <- Some(RuntimeError.oldError "match not matched")


    // If we've reached the end of the instructions, return the result
    match rte with
    | None -> return vmState.registers[vmState.resultReg]
    | Some rte ->
      // TODO
      //return raiseRTE exeState.tracing.callStack rte
      return RuntimeError.toDT rte
  }


and call
  (exeState : ExecutionState)
  (vmState : VMState)
  (thingToCall : Dval)
  (typeArgs : List<TypeReference>)
  (args : NEList<Dval>)
  : Ply<Dval> =
  uply {
    match thingToCall with
    | DFnVal(NamedFn fnName) ->
      let! fn =
        match fnName with
        | FQFnName.Builtin std ->
          Map.find std exeState.fns.builtIn |> Option.map builtInFnToFn |> Ply

        | FQFnName.Package pkg ->
          uply {
            let! fn = exeState.fns.package pkg
            return Option.map packageFnToFn fn
          }

      match fn with
      | Some fn ->
        // let expectedTypeParams = List.length fn.typeParams
        // let expectedArgs = NEList.length fn.parameters

        // let actualTypeArgs = List.length typeArgs
        // let actualArgs = NEList.length args

        // if expectedTypeParams <> actualTypeArgs || expectedArgs <> actualArgs then
        //   ExecutionError.raise
        //     state.tracing.callStack
        //     (ExecutionError.WrongNumberOfFnArgs(
        //       fnToCall,
        //       expectedTypeParams,
        //       expectedArgs,
        //       actualTypeArgs,
        //       actualArgs
        //     ))

        let vmState =
          let boundArgs =
            NEList.map2
              (fun (p : Param) actual -> (p.name, actual))
              fn.parameters
              args
            |> NEList.toList
            |> Map
          { vmState with
              symbolTable = Map.mergeFavoringRight vmState.symbolTable boundArgs }

        let vmState =
          let newlyBoundTypeArgs = List.zip fn.typeParams typeArgs |> Map
          { vmState with
              typeSymbolTable =
                Map.mergeFavoringRight vmState.typeSymbolTable newlyBoundTypeArgs }

        return! execFn exeState vmState fnName fn typeArgs args

      | None ->
        // Functions which aren't available in the runtime (for whatever reason)
        // may have results available in traces. (use case: inspecting a cloud-run trace locally)
        let fnResult =
          exeState.tracing.loadFnResult
            (exeState.tracing.callStack.lastCalled, fnName)
            args

        match fnResult with
        | Some(result, _ts) -> return result
        | None ->
          return
            raiseRTE
              exeState.tracing.callStack
              (RuntimeError.oldError
                $"Function {FQFnName.toString fnName} is not found")

    | _ ->
      debuG "thingToCall" thingToCall
      return DUnit // TODO
  }

and execFn
  (exeState : ExecutionState)
  (vmState : VMState)
  (fnDesc : FQFnName.FQFnName)
  (fn : Fn)
  (typeArgs : List<TypeReference>)
  (args : NEList<Dval>)
  : DvalTask =
  uply {
    let typeArgsResolvedInFn = List.zip fn.typeParams typeArgs |> Map
    let typeSymbolTable =
      Map.mergeFavoringRight vmState.typeSymbolTable typeArgsResolvedInFn

    match! TypeChecker.checkFunctionCall exeState.types typeSymbolTable fn args with
    | Error rte -> return raiseRTE exeState.tracing.callStack rte
    | Ok() ->
      let! result =
        match fn.fn with
        | BuiltInFunction f ->
          let executionPoint = ExecutionPoint.Function fn.name

          exeState.tracing.traceExecutionPoint executionPoint

          let exeState =
            { exeState with tracing.callStack.lastCalled = (executionPoint, None) }

          uply {
            let! result =
              uply {
                try
                  return! f (exeState, vmState, typeArgs, NEList.toList args)
                with e ->
                  match e with
                  | RuntimeErrorException(None, rte) ->
                    // Sometimes it's awkward, in a Builtin fn impl, to pass around the callStack
                    // So we catch the exception here and add the callStack to it so it's handy in error-reporting
                    return raiseRTE exeState.tracing.callStack rte

                  | RuntimeErrorException _ -> return Exception.reraise e

                  | e ->
                    let context : Metadata =
                      [ "fn", fnDesc; "args", args; "typeArgs", typeArgs; "id", id ]
                    exeState.reportException exeState context e
                    // These are arbitrary errors, and could include sensitive
                    // information, so best not to show it to the user. If we'd
                    // like to show it to the user, we should catch it where it happens
                    // and give them a known safe error via a RuntimeError
                    return
                      raiseRTE
                        exeState.tracing.callStack
                        (RuntimeError.oldError "Unknown error")
              }

            if fn.previewable <> Pure then
              // TODO same thing here -- shouldn't require ourselves to pass in lastCalled - `tracing` should just get access to it underneath
              exeState.tracing.storeFnResult
                (exeState.tracing.callStack.lastCalled, fnDesc)
                args
                result

            return result
          }

        | PackageFunction(_id, _instructionsWithContext) ->
          //let _registersNeeded, instructions, resultReg = _instructionsWithContext
          // // maybe this should instead be something like `state.tracing.tracePackageFnCall tlid`?
          // // and the `caller` would be updated by that function? (maybe `caller` is a read-only thing.)
          // let executionPoint = ExecutionPoint.Function(FQFnName.Package id)

          // state.tracing.traceExecutionPoint executionPoint

          // // let state =
          // //   { state with
          // //       tracing.callStack.lastCalled = (executionPoint, Some(Expr.toID body)) }

          // and how can we pass the args in?
          // maybe fns need some LoadVal instructions frontloaded or something? hmm.
          //eval state instructions resultReg
          Ply DUnit // TODO

      match!
        TypeChecker.checkFunctionReturnType exeState.types typeSymbolTable fn result
      with
      | Error rte -> return raiseRTE exeState.tracing.callStack rte
      | Ok() -> return result
  }



and eval (exeState : ExecutionState) (vmState : VMState) : Ply<Dval> =
  execute exeState vmState
