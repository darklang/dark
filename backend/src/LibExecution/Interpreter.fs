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

  | MPEnum(caseName, fields), DEnum(_, _, _, caseNameActual, fieldsActual) ->
    if caseName = caseNameActual then rList fields fieldsActual else false, []

  // Dval didn't match the pattern even in a basic sense
  | MPVariable _, _
  | MPUnit, _
  | MPBool _, _
  | MPInt64 _, _
  | MPUInt64 _, _
  | MPInt8 _, _
  | MPUInt8 _, _
  | MPInt16 _, _
  | MPUInt16 _, _
  | MPInt32 _, _
  | MPUInt32 _, _
  | MPInt128 _, _
  | MPUInt128 _, _
  | MPChar _, _
  | MPString _, _
  | MPFloat _, _
  | MPTuple _, _
  | MPListCons _, _
  | MPList _, _
  | MPEnum _, _ -> false, []



let rec evalConst (threadID : ThreadID) (c : Const) : Dval =
  let r = evalConst threadID

  match c with
  | CUnit -> DUnit
  | CBool b -> DBool b

  | CInt8 i -> DInt8 i
  | CUInt8 i -> DUInt8 i
  | CInt16 i -> DInt16 i
  | CUInt16 i -> DUInt16 i
  | CInt32 i -> DInt32 i
  | CUInt32 i -> DUInt32 i
  | CInt64 i -> DInt64 i
  | CUInt64 i -> DUInt64 i
  | CInt128 i -> DInt128 i
  | CUInt128 i -> DUInt128 i

  | CFloat(sign, w, f) -> DFloat(makeFloat sign w f)

  | CChar c -> DChar c
  | CString s -> DString s

  | CList items -> DList(ValueType.Unknown, (List.map r items))
  | CTuple(first, second, rest) -> DTuple(r first, r second, List.map r rest)
  | CDict items ->
    DDict(ValueType.Unknown, (List.map (Tuple2.mapSecond r) items) |> Map.ofList)

  | CEnum(Ok typeName, caseName, fields) ->
    // TYPESTODO: this uses the original type name, so if it's an alias, it won't be equal to the
    DEnum(typeName, typeName, VT.typeArgsTODO, caseName, List.map r fields)

  | CEnum(Error nre, _caseName, _fields) ->
    // TODO: ConstNotFound or something
    raiseRTE threadID (RuntimeError.NameResolution nre)


let execute (exeState : ExecutionState) (vm : VMState) : Ply<Dval> =
  uply {
    let raiseRTE rte = raiseRTE vm.threadID rte

    let mutable finalResult : Dval option = None

    while Map.containsKey vm.currentFrameID vm.callFrames do
      let currentFrame = Map.findUnsafe vm.currentFrameID vm.callFrames

      let mutable counter = currentFrame.programCounter
      let registers = currentFrame.registers


      let! instrData =
        match currentFrame.context with
        | Source -> Ply vm.rootInstrData

        | Lambda(parentContext, lambdaID) ->
          let lambda =
            (match Map.tryFind (parentContext, lambdaID) vm.lambdaInstrCache with
             | Some l -> l
             | None ->
               match Map.tryFind (Source, lambdaID) vm.lambdaInstrCache with
               | Some l -> l
               | None -> raiseRTE (RTE.VariableNotFound "lambda not found")) // TODO better error
            |> _.instructions

          { instructions = List.toArray lambda.instructions
            resultReg = lambda.resultIn }
          |> Ply

        | PackageFn fn ->
          uply {
            match Map.find fn vm.packageFnInstrCache with
            | Some fn -> return fn
            | None ->
              match! exeState.fns.package fn with
              | Some fn ->
                let instrData =
                  { instructions = List.toArray fn.body.instructions
                    resultReg = fn.body.resultIn }
                vm.packageFnInstrCache <-
                  Map.add fn.id instrData vm.packageFnInstrCache
                return instrData

              | None -> return raiseRTE (RTE.FnNotFound(FQFnName.Package fn))
          }


      let mutable frameToPush = None

      while counter < instrData.instructions.Length && frameToPush = None do

        let inst = instrData.instructions[counter]
        match inst with

        // == Simple register operations ==
        | LoadVal(reg, value) -> registers[reg] <- value

        | CopyVal(copyTo, copyFrom) -> registers[copyTo] <- registers[copyFrom]

        | Or(createTo, left, right) ->
          // match registers[left], registers[right] with
          // | DBool l, DBool r -> registers[createTo] <- DBool(l || r)
          // | l, r ->
          //   RTE.Bools.OrOnlySupportsBooleans(Dval.toValueType l, Dval.toValueType r)
          //   |> RTE.Bool
          //   |> raiseRTE
          match registers[left] with
          | DBool true -> registers[createTo] <- DBool true
          | DBool false ->
            match registers[right] with
            | DBool true -> registers[createTo] <- DBool true
            | DBool false -> registers[createTo] <- DBool false
            | r ->
              RTE.Bools.OrOnlySupportsBooleans(VT.bool, Dval.toValueType r)
              |> RTE.Bool
              |> raiseRTE
          | l ->
            let r = registers[right]
            RTE.Bools.OrOnlySupportsBooleans(Dval.toValueType l, Dval.toValueType r)
            |> RTE.Bool
            |> raiseRTE

        | And(createTo, left, right) ->
          match registers[left] with
          | DBool false -> registers[createTo] <- DBool false
          | DBool true ->
            match registers[right] with
            | DBool true -> registers[createTo] <- DBool true
            | DBool false -> registers[createTo] <- DBool false
            | r ->
              RTE.Bools.AndOnlySupportsBooleans(VT.bool, Dval.toValueType r)
              |> RTE.Bool
              |> raiseRTE
          | l ->
            let r = registers[right]
            RTE.Bools.AndOnlySupportsBooleans(Dval.toValueType l, Dval.toValueType r)
            |> RTE.Bool
            |> raiseRTE


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


        | CloneRecordWithUpdates(targetReg, originalRecordReg, updates) ->
          let originalRecord = registers[originalRecordReg]

          match originalRecord with
          | DRecord(_, typeName, typeArgs, originalFields) ->
            // TODO: type-saftety
            let fields =
              List.fold
                (fun acc (fieldName, valueReg) ->
                  Map.add fieldName (registers[valueReg]) acc)
                originalFields
                updates

            registers[targetReg] <- DRecord(typeName, typeName, typeArgs, fields)

          | dv ->
            Dval.toValueType dv
            |> RTE.Records.UpdateNotRecord
            |> RTE.Record
            |> raiseRTE


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
        | CreateEnum(enumReg, typeName, typeArgs, caseName, fields) ->
          // TODO: safe dval creation
          let fields = fields |> List.map (fun (valueReg) -> registers[valueReg])
          let! enum =
            TypeChecker.DvalCreator.enum
              vm.threadID
              exeState.types
              typeName
              typeArgs
              caseName
              fields
          registers[enumReg] <- enum


        | LoadConstant(createTo, name) ->
          match name with
          | FQConstantName.Builtin builtin ->
            match Map.find builtin exeState.constants.builtIn with
            | Some c -> registers[createTo] <- c.body
            | None -> raiseRTE (RTE.ConstNotFound(FQConstantName.Builtin builtin))

          | FQConstantName.Package pkg ->
            match! exeState.constants.package pkg with
            | Some c -> registers[createTo] <- evalConst vm.threadID c.body
            | None -> raiseRTE (RTE.ConstNotFound(FQConstantName.Package pkg))


        | CreateLambda(lambdaReg, impl) ->
          vm.lambdaInstrCache <-
            vm.lambdaInstrCache
            |> Map.add (currentFrame.context, impl.exprId) impl
            |> Map.add (Source, impl.exprId) impl

          registers[lambdaReg] <-
            { exprId = impl.exprId
              closedRegisters =
                impl.registersToCloseOver
                |> List.map (fun (parentReg, childReg) ->
                  childReg, registers[parentReg])
              argsSoFar = [] }
            |> AppLambda
            |> DApplicable



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
          | AppLambda applicableLambda ->
            let foundLambda =
              match
                Map.tryFind
                  (currentFrame.context, applicableLambda.exprId)
                  vm.lambdaInstrCache
              with
              | Some lambda -> lambda
              | None ->
                match
                  Map.tryFind (Source, applicableLambda.exprId) vm.lambdaInstrCache
                with
                | Some lambda -> lambda
                | None -> raiseRTE (RTE.VariableNotFound "lambda not found") // TODO better error

            let allArgs = applicableLambda.argsSoFar @ newArgDvals

            let argCount = List.length allArgs
            let paramCount = NEList.length foundLambda.patterns

            //let typeArgCount = List.length typeArgs
            // TODO: fail if we try to apply a lambda with type args

            if argCount = paramCount then
              let newFrame =
                { id = guuid ()
                  parent = Some(vm.currentFrameID, putResultIn, counter + 1)
                  programCounter = 0
                  registers =
                    let r = Array.zeroCreate foundLambda.instructions.registerCount

                    allArgs |> List.iteri (fun i arg -> r[i] <- arg)

                    applicableLambda.closedRegisters
                    |> List.iter (fun (reg, value) -> r[reg] <- value)

                    r
                  context = Lambda(currentFrame.context, applicableLambda.exprId) }

              frameToPush <- Some newFrame

            else if argCount > paramCount then
              // TODO
              RTE.MatchUnmatched |> raiseRTE
            else
              registers[putResultIn] <-
                { applicableLambda with argsSoFar = allArgs }
                |> AppLambda
                |> DApplicable

          | AppNamedFn applicable ->
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
                        |> AppNamedFn
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
                  frameToPush <-
                    { id = guuid ()
                      parent = Some(vm.currentFrameID, putResultIn, counter + 1)
                      programCounter = 0
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
                    { applicable with argsSoFar = allArgs }
                    |> AppNamedFn
                    |> DApplicable

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
          parentFrame.programCounter <- pcOfParent

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
