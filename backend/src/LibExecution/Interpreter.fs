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
  : bool * List<Register * Dval> =
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
  | LPVariable extractTo, dv -> true, [ (extractTo, dv) ]
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
  : bool * List<Register * Dval> =
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
  | MPVariable reg, dv -> true, [ (reg, dv) ]

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
    let currentFrame = Map.findUnsafe vm.currentFrameID vm.callFrames

    let mutable counter = currentFrame.pc // what instruction (by index) we're on
    let registers = currentFrame.registers

    let raiseRTE rte = raiseRTE vm.threadID rte

    while counter < currentFrame.instructions.Length do

      match currentFrame.instructions[counter] with

      // == Simple register operations ==
      | LoadVal(reg, value) ->
        registers[reg] <- value
        counter <- counter + 1

      | CopyVal(copyTo, copyFrom) ->
        registers[copyTo] <- registers[copyFrom]
        counter <- counter + 1


      // // == Working with Variables ==
      | CheckLetPatternAndExtractVars(valueReg, pat) ->
        let dv = registers[valueReg]
        let doesMatch, registersToAssign = checkAndExtractLetPattern pat dv

        if doesMatch then
          registersToAssign
          |> List.iter (fun (reg, value) -> registers[reg] <- value)

          counter <- counter + 1
        else
          raiseRTE (RTE.Let(RTE.Lets.PatternDoesNotMatch(dv, pat)))


      | VarNotFound varName -> raiseRTE (RTE.VariableNotFound varName)


      // == Working with Basic Types ==
      | CreateString(targetReg, segments) ->
        let sb = new System.Text.StringBuilder()

        segments
        |> List.iter (fun seg ->
          match seg with
          | Text s -> sb.Append s |> ignore<System.Text.StringBuilder>
          | Interpolated reg ->
            match registers[reg] with
            | DString s -> sb.Append s |> ignore<System.Text.StringBuilder>
            | _ -> raiseRTE (RTE.String RTE.Strings.Error.InvalidStringAppend))

        registers[targetReg] <- DString(sb.ToString())
        counter <- counter + 1


      // == Flow Control ==
      // -- Jumps --
      | JumpBy jumpBy -> counter <- counter + jumpBy + 1

      | JumpByIfFalse(jumpBy, condReg) ->
        match registers[condReg] with
        | DBool false -> counter <- counter + jumpBy + 1
        | DBool true -> counter <- counter + 1
        | dv ->
          let vt = Dval.toValueType dv
          raiseRTE (RTE.Bool(RTE.Bools.ConditionRequiresBool(vt, dv)))

      // -- Match --
      | CheckMatchPatternAndExtractVars(valueReg, pat, failJump) ->
        let doesMatch, registersToAssign =
          checkAndExtractMatchPattern pat registers[valueReg]

        if doesMatch then
          registersToAssign
          |> List.iter (fun (reg, value) -> registers[reg] <- value)
          counter <- counter + 1
        else
          counter <- counter + failJump + 1

      | MatchUnmatched -> raiseRTE RTE.MatchUnmatched


      // == Working with Collections ==
      | CreateList(listReg, itemsToAddRegs) ->
        // CLEANUP reference registers directly in DvalCreator.list,
        // so we don't have to copy things
        let itemsToAdd = itemsToAddRegs |> List.map (fun r -> registers[r])
        registers[listReg] <-
          TypeChecker.DvalCreator.list vm.threadID VT.unknown itemsToAdd
        counter <- counter + 1

      | CreateDict(dictReg, entries) ->
        // CLEANUP reference registers directly in DvalCreator.dict,
        // so we don't have to copy things
        let entries =
          entries |> List.map (fun (key, valueReg) -> (key, registers[valueReg]))
        registers[dictReg] <-
          TypeChecker.DvalCreator.dict vm.threadID VT.unknown entries
        counter <- counter + 1

      | CreateTuple(tupleReg, firstReg, secondReg, theRestRegs) ->
        let first = registers[firstReg]
        let second = registers[secondReg]
        let theRest = theRestRegs |> List.map (fun r -> registers[r])
        registers[tupleReg] <- DTuple(first, second, theRest)
        counter <- counter + 1


      // == Working with Custom Data ==
      // -- Records --
      | CreateRecord(recordReg, typeName, typeArgs, fields) ->
        let fields =
          fields |> List.map (fun (name, valueReg) -> (name, registers[valueReg]))

        let! record =
          TypeChecker.DvalCreator.record
            vm.threadID
            exeState.types
            typeName
            typeArgs
            fields

        registers[recordReg] <- record
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
        match registers[recordReg] with
        | DRecord(_, _, _, fields) ->
          match Map.find fieldName fields with
          | Some value ->
            registers[targetReg] <- value
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
        let fields = fields |> List.map (fun (valueReg) -> registers[valueReg])
        registers[enumReg] <- DEnum(typeName, typeName, [], caseName, fields)
        counter <- counter + 1

      // | CreateLambda(lambdaReg, impl) ->
      //   vm.lambdas <- Map.add impl.exprId impl vm.lambdas
      //   vm.registers[lambdaReg] <-
      //     { exprId = impl.exprId; symtable = Map.empty; argsSoFar = [] }
      //     |> Applicable.Lambda
      //     |> DApplicable
      //   counter <- counter + 1


      // // == Working with things that Apply (fns, lambdas) ==
      // // `add (increment 1L) (3L)` and store results in `putResultIn`
      // | Apply(putResultIn, thingToCallReg, _typeArgs, argRegs) ->
      //   let thingToCall = vm.registers[thingToCallReg]

      //   let result =
      //     match thingToCall with
      //     | DApplicable applicable ->
      //       match applicable with
      //       | Lambda lambda ->
      //         let impl = Map.findUnsafe lambda.exprId vm.lambdas

      //         // TODO: too many args
      //         if
      //           (NEList.length impl.patterns) = (lambda.argsSoFar.Length
      //                                            + NEList.length argRegs)
      //         then
      //           DUnit // TODO
      //         else
      //           // TODO
      //           DApplicable applicable

      //       | NamedFn _namedFn ->
      //         // TODO
      //         DApplicable applicable

      //     | _ ->
      //       RTE.ExpectedApplicableButNot(Dval.toValueType thingToCall, thingToCall)
      //       |> raiseRTE

      //   vm.registers[putResultIn] <- result

      //   counter <- counter + 1


      | RaiseNRE nre -> raiseRTE (RTE.NameResolution nre)


    // If we've reached the end of the instructions, return the result
    return registers[currentFrame.resultReg]
  }


and eval (exeState : ExecutionState) (vmState : VMState) : Ply<Dval> =
  execute exeState vmState
