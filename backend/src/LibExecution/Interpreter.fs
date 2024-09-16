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
let execute (exeState : ExecutionState) (vm : VMState) : Ply<Dval> =
  uply {
    let raiseRTE rte = raiseRTE vm.threadID rte

    //while Map.count vm.callFrames > 1 do

    // If there's a parent frame to return to, continue execution
    // let mutable continueExecution = true
    let mutable finalResult : Dval option = None

    // while continueExecution && Map.containsKey vm.currentFrameID vm.callFrames do
    while Map.containsKey vm.currentFrameID vm.callFrames do
      let currentFrame = Map.findUnsafe vm.currentFrameID vm.callFrames

      let mutable counter = currentFrame.pc
      let registers = currentFrame.registers


      let! instrData =
        match currentFrame.context with
        | Source -> Ply vm.sourceInfo

        | PackageFn fn ->
          uply {
            match Map.find fn vm.packageFns with
            | Some fn -> return fn
            | None ->
              match! exeState.fns.package fn with
              | Some fn ->
                return
                  { instructions = List.toArray fn.body.instructions
                    resultReg = fn.body.resultIn }
              | None -> return raiseRTE (RTE.FnNotFound(FQFnName.Package fn))
          }


      let mutable frameToPush = None

      while counter < instrData.instructions.Length && frameToPush = None do

        let inst = instrData.instructions[counter]
        match inst with

        // == Simple register operations ==
        | LoadVal(reg, value) -> registers[reg] <- value

        | CopyVal(copyTo, copyFrom) -> registers[copyTo] <- registers[copyFrom]


        // == Working with Variables ==
        | CheckLetPatternAndExtractVars(valueReg, pat) ->
          let dv = registers[valueReg]
          let doesMatch, registersToAssign = checkAndExtractLetPattern pat dv

          if doesMatch then
            registersToAssign
            |> List.iter (fun (reg, value) -> registers[reg] <- value)
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


        // == Flow Control ==
        // -- Jumps --
        | JumpBy jumpBy -> counter <- counter + jumpBy

        | JumpByIfFalse(jumpBy, condReg) ->
          match registers[condReg] with
          | DBool false -> counter <- counter + jumpBy
          | DBool true -> ()
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
          else
            counter <- counter + failJump

        | MatchUnmatched -> raiseRTE RTE.MatchUnmatched


        // == Working with Collections ==
        | CreateList(listReg, itemsToAddRegs) ->
          // CLEANUP reference registers directly in DvalCreator.list,
          // so we don't have to copy things
          let itemsToAdd = itemsToAddRegs |> List.map (fun r -> registers[r])
          registers[listReg] <-
            TypeChecker.DvalCreator.list vm.threadID VT.unknown itemsToAdd

        | CreateDict(dictReg, entries) ->
          // CLEANUP reference registers directly in DvalCreator.dict,
          // so we don't have to copy things
          let entries =
            entries |> List.map (fun (key, valueReg) -> (key, registers[valueReg]))
          registers[dictReg] <-
            TypeChecker.DvalCreator.dict vm.threadID VT.unknown entries

        | CreateTuple(tupleReg, firstReg, secondReg, theRestRegs) ->
          let first = registers[firstReg]
          let second = registers[secondReg]
          let theRest = theRestRegs |> List.map (fun r -> registers[r])
          registers[tupleReg] <- DTuple(first, second, theRest)


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

        | GetRecordField(targetReg, recordReg, fieldName) ->
          match registers[recordReg] with
          | DRecord(_, _, _, fields) ->
            match Map.find fieldName fields with
            | Some value -> registers[targetReg] <- value
            | None ->
              RTE.Records.FieldAccessFieldNotFound fieldName
              |> RTE.Record
              |> raiseRTE
          | dv ->
            RTE.Records.FieldAccessNotRecord(Dval.toValueType dv)
            |> RTE.Record
            |> raiseRTE

        // -- Enums --
        | CreateEnum(enumReg, typeName, _typeArgs, caseName, fields) ->
          // TODO: safe dval creation
          let fields = fields |> List.map (fun (valueReg) -> registers[valueReg])
          registers[enumReg] <- DEnum(typeName, typeName, [], caseName, fields)

        // | CreateLambda(lambdaReg, impl) ->
        //   vm.lambdas <- Map.add impl.exprId impl vm.lambdas
        //   vm.registers[lambdaReg] <-
        //     { exprId = impl.exprId; symtable = Map.empty; argsSoFar = [] }
        //     |> Applicable.Lambda
        //     |> DApplicable


        // == Working with things that Apply (fns, lambdas) ==
        // `add (increment 1L) (3L)` and store results in `putResultIn`
        | Apply(putResultIn, thingToCallReg, typeArgs, newArgRegs) ->
          // CLEANUP
          // only the first apply of an applicable should be allowed to provide type args

          // further constraint: only named fns can have type args? no, see below.
          // let x = Json.parse
          // x<Int64> "3"

          let thingToCall = registers[thingToCallReg]

          let newArgDvals =
            newArgRegs |> NEList.toList |> List.map (fun r -> registers[r])

          let applicable =
            match thingToCall with
            | DApplicable applicable -> applicable
            | _ ->
              raiseRTE (
                RTE.ExpectedApplicableButNot(
                  Dval.toValueType thingToCall,
                  thingToCall
                )
              )

          match applicable with
          // | Lambda lambda -> DApplicable applicable

          | NamedFn applicable ->
            // TODO: typechecking
            // TODO: reduce duplication between branches
            match applicable.name with
            | FQFnName.Builtin builtin ->
              match Map.find builtin exeState.fns.builtIn with
              | None -> return RTE.FnNotFound(FQFnName.Builtin builtin) |> raiseRTE
              | Some fn ->
                let allArgs = applicable.argsSoFar @ newArgDvals

                let argCount = List.length allArgs
                let paramCount = List.length fn.parameters

                let typeParamCount = List.length fn.typeParams
                let typeArgCount = List.length typeArgs
                // TODO: error on these not matching^, too.

                let! result =
                  uply {
                    if argCount = paramCount then
                      let! result = fn.fn (exeState, vm, [], allArgs)
                      return result
                    else if argCount > paramCount then
                      return
                        RTE.TooManyArgs(
                          FQFnName.Builtin fn.name,
                          typeParamCount,
                          typeArgCount,
                          paramCount,
                          argCount
                        )
                        |> raiseRTE
                    else
                      return
                        { applicable with argsSoFar = allArgs }
                        |> NamedFn
                        |> DApplicable
                  }

                registers[putResultIn] <- result

            | FQFnName.Package pkg ->
              match! exeState.fns.package pkg with
              | None -> return RTE.FnNotFound(FQFnName.Package pkg) |> raiseRTE
              | Some fn ->
                let allArgs = applicable.argsSoFar @ newArgDvals

                let argCount = List.length allArgs
                let paramCount = NEList.length fn.parameters

                let typeParamCount = List.length fn.typeParams
                let typeArgCount = List.length typeArgs
                // TODO: error on these not matching^, too.

                if argCount = paramCount then
                  // fun with call frames
                  frameToPush <-
                    { id = guuid ()
                      parent = Some(vm.currentFrameID, putResultIn, counter + 1)
                      pc = 0
                      registers =
                        let r = Array.zeroCreate fn.body.registerCount
                        allArgs |> List.iteri (fun i arg -> r[i] <- arg)
                        r
                      context = PackageFn fn.id }
                    |> Some

                else if argCount > paramCount then
                  RTE.TooManyArgs(
                    FQFnName.Package fn.id,
                    typeParamCount,
                    typeArgCount,
                    paramCount,
                    argCount
                  )
                  |> raiseRTE
                else
                  registers[putResultIn] <-
                    { applicable with argsSoFar = allArgs } |> NamedFn |> DApplicable

        | RaiseNRE nre -> raiseRTE (RTE.NameResolution nre)

        counter <- counter + 1


      match frameToPush with
      | None ->
        // we are at the end of the instructions of the current frame
        let resultOfFrame = registers[instrData.resultReg]
        match currentFrame.parent with
        | Some(parentID, regOfParentToPutResultInto, pcOfParent) ->
          vm.callFrames <- Map.remove vm.currentFrameID vm.callFrames
          vm.currentFrameID <- parentID
          let parentFrame = Map.findUnsafe parentID vm.callFrames
          parentFrame.registers[regOfParentToPutResultInto] <- resultOfFrame
          parentFrame.pc <- pcOfParent

        | None ->
          vm.callFrames <- Map.remove vm.currentFrameID vm.callFrames
          finalResult <- Some resultOfFrame

      | Some newFrame ->
        vm.callFrames <- Map.add newFrame.id newFrame vm.callFrames
        vm.currentFrameID <- newFrame.id


    // If we've reached the end of the instructions, return the result
    match finalResult with
    | Some dv -> return dv
    | None -> return raiseRTE RTE.MatchUnmatched // TODO better error

  }
