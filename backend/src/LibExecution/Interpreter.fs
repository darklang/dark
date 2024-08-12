/// Interprets Dark instructions resulting in (tasks of) Dvals
module LibExecution.Interpreter

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine.Unsafe

open Prelude
open RuntimeTypes


/// TODO: don't pass ExecutionState around so much?
/// The parts that change, (e.g. `st` and `tst`) should probably all be part of VMState
///
/// Maybe rename ExecutionState to something else
/// , like ExecutionContext or Execution
///
/// TODO potentially make this a loop instead of recursive
let rec execute
  (exeState : ExecutionState)
  (vmState : VMState)
  (counter : int)
  : Ply<Dval> =
  uply {
    let instructions = vmState.instructions
    if counter >= instructions.Length then
      // is this OK?
      return vmState.registers[vmState.resultReg]
    else
      let instruction = instructions[counter]

      match instruction with
      // `1L` -> next register
      | LoadVal(reg, value) ->
        vmState.registers[reg] <- value
        return! execute exeState vmState (counter + 1)

      // `let x = 1`
      | SetVar(varName, loadFrom) ->
        let value = vmState.registers[loadFrom]
        let vmState =
          { vmState with symbolTable = Map.add varName value vmState.symbolTable }
        return! execute exeState vmState (counter + 1)

      // later, `x`
      | GetVar(loadTo, varName) ->
        let value =
          Map.find varName vmState.symbolTable
          // TODO: handle missing variable
          //return errStr callStack $"There is no variable named: {name}"
          |> Option.defaultValue DUnit

        vmState.registers[loadTo] <- value

        return! execute exeState vmState (counter + 1)


      // `add (increment 1L) (3L)` and store results in `putResultIn`
      // At this point, the 'increment' has already been evaluated.
      // But maybe that's something we should change, (CLEANUP)
      // so that we don't execute things until they're needed
      | Apply(putResultIn, thingToCallReg, typeArgs, argRegs) ->
        // should we instead pass in register indices? probably...
        let args = argRegs |> NEList.map (fun r -> vmState.registers[r])
        let thingToCall = vmState.registers[thingToCallReg]
        let! result = call exeState vmState thingToCall typeArgs args

        vmState.registers[putResultIn] <- result

        return! execute exeState vmState (counter + 1)

      | AddItemToList(listReg, itemToAddReg) ->
        match vmState.registers[listReg] with
        | DList(vt, list) ->
          // TODO: type checking of item-add; adjust vt

          // Had:
          //   let! results = Ply.List.mapSequentially (eval state) exprs
          //   return TypeChecker.DvalCreator.list callStack VT.unknown results

          let itemToAdd = vmState.registers[itemToAddReg]
          vmState.registers[listReg] <- DList(vt, list @ [ itemToAdd ])
          return! execute exeState vmState (counter + 1)
        | _ -> return DString "TODO can't operate list-add to a non-list"

      | CreateTuple(tupleReg, firstReg, secondReg, theRestRegs) ->
        let first = vmState.registers[firstReg]
        let second = vmState.registers[secondReg]
        let theRest = theRestRegs |> List.map (fun r -> vmState.registers[r])
        vmState.registers[tupleReg] <- DTuple(first, second, theRest)
        return! execute exeState vmState (counter + 1)

      | AddDictEntry(dictReg, key, valueReg) ->
        match vmState.registers[dictReg] with
        | DDict(vt, entries) ->
          // TODO: type checking of key and value; adjust vt
          let value = vmState.registers[valueReg]
          vmState.registers[dictReg] <- DDict(vt, Map.add key value entries)
          return! execute exeState vmState (counter + 1)
        | _ -> return DString "TODO can't operate dict-add to a non-dict"


      | AppendString(targetReg, sourceReg) ->
        match vmState.registers[targetReg], vmState.registers[sourceReg] with
        | DString target, DString source ->
          vmState.registers[targetReg] <- DString(target + source)
          return! execute exeState vmState (counter + 1)
        | _, _ -> return DString "Error: Invalid string-append attempt"


      | JumpByIfFalse(jumpBy, condReg) ->
        match vmState.registers[condReg] with
        | DBool false -> return! execute exeState vmState (counter + jumpBy + 1)
        | DBool true -> return! execute exeState vmState (counter + 1)
        | _ -> return DString "Error: Jump condition must be a boolean"

      | JumpBy jumpBy -> return! execute exeState vmState (counter + jumpBy + 1)

      | CopyVal(copyTo, copyFrom) ->
        vmState.registers[copyTo] <- vmState.registers[copyFrom]
        return! execute exeState vmState (counter + 1)

      | ExtractTupleItems(extractFrom, firstReg, secondReg, restRegs) ->
        match vmState.registers[extractFrom] with
        | DTuple(first, second, rest) ->
          vmState.registers[firstReg] <- first
          vmState.registers[secondReg] <- second

          List.zip restRegs rest
          |> List.iter (fun (reg, value) -> vmState.registers[reg] <- value)

          return! execute exeState vmState (counter + 1)
        | _ -> return DString "Error: Expected a tuple for decomposition"

      | Fail _rte -> return DUnit // TODO
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
  execute exeState vmState 0
